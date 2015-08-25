{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Wallet.Server
( runSPVServer
, stopSPVServer
) where

import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)
import System.ZMQ4 
    ( Rep(..), Socket, bind, receive, send
    ,  withContext, withSocket
    )

import Control.Applicative ((<$>))
import Control.Monad (when, void, unless, forM, forM_, forever, liftM, join)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Control 
    ( StM, MonadBaseControl, control
    , liftBaseOp, liftBaseOp_, liftBaseWith, restoreM
    , liftBaseOpDiscard
    )
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Control.DeepSeq (NFData(..), ($!!))
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import Control.Concurrent.STM (TVar, atomically, retry, readTVar, newTVarIO)
import Control.Concurrent.Async.Lifted 
    (async, link, mapConcurrently, cancel, waitAnyCancel)
import Control.Exception.Lifted
    (SomeException(..), ErrorCall(..), catches, finally)
import qualified Control.Exception.Lifted as E (Handler(..))
import qualified Control.Concurrent.MSem as Sem (MSem, new)
import Control.Monad.Logger 
    ( MonadLogger
    , runStdoutLoggingT
    , LogLevel(..)
    , LoggingT(..)
    , logError
    , logDebug
    , logWarn
    , logInfo
    , filterLogger
    )

import qualified Data.HashMap.Strict as H (lookup)
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.Aeson (Value, toJSON, decode, encode)
import Data.Conduit (Sink, await, awaitForever, ($$))
import Data.Conduit.TMChan (TBMChan, sourceTBMChan)
import Data.Word (Word32)
import qualified Data.Map.Strict as M 
    (Map, unionWith, null, toList, empty, fromListWith, assocs, elems)

import Database.Persist (get)
import Database.Persist.Sql (ConnectionPool, runMigration)
import qualified Database.LevelDB.Base as DB (Options(..), open, defaultOptions)

import Database.Esqueleto 
    ( Esqueleto, SqlQuery, SqlExpr, SqlBackend, SqlEntity
    , InnerJoin(..), LeftOuterJoin(..), OrderBy, update, sum_, groupBy
    , select, from, where_, val, valList, sub_select, countRows, count
    , orderBy, limit, asc, desc, set, offset, selectSource, updateCount
    , subList_select, in_, unValue, max_, not_, coalesceDefault, just, on
    , case_, when_, then_, else_, distinct
    , (^.), (=.), (==.), (&&.), (||.), (<.)
    , (<=.), (>.), (>=.), (-.), (*.), (?.), (!=.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    , getBy, insertUnique, updateGet, replace, get, insertMany_, insert_
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Node.Peer
import Network.Haskoin.Node.BlockChain
import Network.Haskoin.Node.STM
import Network.Haskoin.Node.HeaderTree

import Network.Haskoin.Wallet.KeyRing
import Network.Haskoin.Wallet.Transaction
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Settings
import Network.Haskoin.Wallet.Server.Handler
import Network.Haskoin.Wallet.Database

data EventSession = EventSession
    { eventBatchSize :: !Int 
    , eventNewAddrs  :: !(M.Map KeyRingAccountId Word32)
    }
    deriving (Eq, Show, Read)

instance NFData EventSession where
    rnf EventSession{..} =
        rnf eventBatchSize `seq`
        rnf (M.elems eventNewAddrs)

runSPVServer :: Config -> IO ()
runSPVServer cfg = maybeDetach cfg $ do -- start the server process
    -- Initialize the database
    (sem, pool) <- initDatabase cfg
    -- Check the operation mode of the server.
    run $ case configMode cfg of
        -- In this mode, we do not launch an SPV node. We only accept
        -- client requests through the ZMQ API.
        SPVOffline -> runWalletApp $ HandlerSession cfg pool Nothing sem
        -- In this mode, we launch the client ZMQ API and we sync the
        -- wallet database with an SPV node.
        SPVOnline -> do
            -- Initialize the node state
            nodeState <- getNodeState fp opts
            -- Spin up the node threads
            as <- mapM async 
                -- Start the SPV node
                [ runNodeT nodeState $ do
                    -- Get our bloom filter
                    bloom <- liftM fst3 $ runDBPool sem pool getBloomFilter
                    startSPVNode hosts bloom
                -- Merkle block synchronization
                , runMerkleSync nodeState sem pool
                -- Import solo transactions as they arrive from peers
                , runNodeT nodeState $ txSource $$ processTx sem pool
                -- Respond to transaction GetData requests
                , runNodeT nodeState $
                    handleGetData $ runDBPool sem pool . getTx
                -- Re-broadcast pending transactions
                , broadcastPendingTxs nodeState sem pool
                -- Run the ZMQ API server
                , runWalletApp $ HandlerSession cfg pool (Just nodeState) sem
                ]
            waitAnyCancel as
            return ()
  where
    -- Setup logging monads
    run               = runResourceT . runLogging
    runLogging        = runStdoutLoggingT . filterLogger logFilter
    logFilter _ level = level >= configLogLevel cfg
        -- Bitcoin nodes to connect to
    nodes = fromMaybe
        (error $ "BTC nodes for " ++ networkName ++ " not found")
        (pack networkName `H.lookup` configBTCNodes cfg)
    hosts = map (uncurry PeerHost) nodes
    -- LevelDB options
    fp = "headertree"
    opts = DB.defaultOptions { DB.createIfMissing = True
                             , DB.cacheSize       = 2048
                             }
    -- Run the merkle syncing thread
    runMerkleSync nodeState sem pool = runNodeT nodeState $ do
        $(logDebug) "Waiting for a valid bloom filter for merkle downloads..."

        -- Only download merkles if we have a valid bloom filter
        _ <- atomicallyNodeT waitBloomFilter

        -- Provide a fast catchup time if we are at height 0
        fcM <- liftM (fmap adjustFCTime) $ runDBPool sem pool $ do
            (_, h) <- getBestBlock
            if h == 0 then firstAddrTime else return Nothing
        maybe (return ()) (atomicallyNodeT . rescanTs) fcM
            
        -- Start the merkle sync
        merkleSync sem pool 500 
    -- Run a thread that will re-broadcast pending transactions 
    broadcastPendingTxs nodeState sem pool = runNodeT nodeState $ forever $ do
        -- Wait until we are synced
        atomicallyNodeT $ do
            synced <- areBlocksSynced
            if synced then return () else lift retry
        -- Send an INV for those transactions to all peers
        broadcastTxs =<< runDBPool sem pool (getPendingTxs 100)
        -- Wait until we are not synced
        atomicallyNodeT $ do
            synced <- areBlocksSynced
            if synced then lift retry else return ()
    processTx sem pool = awaitForever $ \tx -> lift $ do
        (_, newAddrs) <- runDBPool sem pool $ importNetTx tx
        unless (null newAddrs) $ do
            $(logInfo) $ pack $ unwords
                [ "Generated", show $ length newAddrs
                , "new addresses while importing the tx." 
                , "Updating the bloom filter"
                ]
            (bloom, _, _) <- runDBPool sem pool getBloomFilter
            atomicallyNodeT $ sendBloomFilter bloom

initDatabase :: Config -> IO (Sem.MSem Int, ConnectionPool)
initDatabase cfg = do
    -- Create a semaphore with 1 resource
    sem <- Sem.new 1
    -- Create a database pool
    let dbCfg = fromMaybe
            (error $ "DB config settings for " ++ networkName ++ " not found")
            (pack networkName `H.lookup` configDatabase cfg)
    pool <- getDatabasePool dbCfg
    -- Initialize wallet database
    runDBPool sem pool $ do
        _ <- runMigration migrateWallet 
        initWallet $ configBloomFP cfg
    -- Return the semaphrone and the connection pool
    return (sem, pool)

merkleSync 
    :: ( MonadLogger m
       , MonadIO m
       , MonadBaseControl IO m
       , MonadThrow m
       , MonadResource m
       )
    => Sem.MSem Int
    -> ConnectionPool
    -> Int
    -> NodeT m ()
merkleSync sem pool bSize = do
    -- Get our best block 
    (best, merkleHeight) <- runDBPool sem pool getBestBlock
    $(logDebug) "Starting merkle batch download"
    -- Wait for a new block or a rescan
    (action, source) <- merkleDownload best bSize
    $(logDebug) "Received a merkle action and source. Processing the source..."

    -- Read and process the data from the source
    (lastMerkleM, mTxsAcc, aMap) <- source $$ go Nothing [] M.empty
    $(logDebug) "Merkle source processed and closed"

    -- Send a new bloom filter to our peers if new addresses were generated
    unless (M.null aMap) $ do
        $(logInfo) $ pack $ unwords
            [ "Generated", show $ sum $ M.elems aMap
            , "new addresses while importing the merkle block." 
            , "Sending our bloom filter."
            ]
        (bloom, _, _) <- runDBPool sem pool getBloomFilter
        atomicallyNodeT $ sendBloomFilter bloom

    -- Check if we should rescan the current merkle batch
    $(logDebug) "Checking if we need to rescan the current batch..."
    rescan <- lift $ shouldRescan aMap
    when rescan $ $(logDebug) "We need to rescan the current batch"
    -- Compute the new batch size
    let newBSize | rescan    = max 1 $ bSize `div` 2
                 | otherwise = min 500 $ bSize + max 1 (bSize `div` 20)

    when (newBSize /= bSize) $ $(logDebug) $ pack $ unwords
        [ "Changing block batch size from", show bSize, "to", show newBSize ]

    -- Did we receive all the merkles that we asked for ?
    let missing = (headerHash <$> lastMerkleM) /= 
                  Just (nodeBlockHash $ last $ actionNewNodes action)

    when missing $ $(logWarn) $ pack $ unwords
        [ "Merkle block stream closed prematurely"
        , show lastMerkleM
        ]

    -- TODO: We could still salvage a partially received batch
    unless (rescan || missing) $ do
        $(logDebug) "Importing merkles into the wallet..."
        -- Confirm the transactions
        runDBPool sem pool $ importMerkles action mTxsAcc
        $(logDebug) "Done importing merkles into the wallet"
        logBlockChainAction action

    merkleSync sem pool newBSize 
  where
    go lastMerkleM mTxsAcc aMap = await >>= \resM -> case resM of 
        Just (Right tx) -> do
            $(logDebug) $ pack $ unwords
                [ "Importing merkle tx", encodeTxHashLE $ txHash tx ]
            (_, newAddrs) <- lift $ runDBPool sem pool $ importNetTx tx
            $(logDebug) $ pack $ unwords
                [ "Generated", show $ length newAddrs
                , "new addresses while importing tx"
                , encodeTxHashLE $ txHash tx
                ]
            let newMap = M.unionWith (+) aMap $ groupByAcc newAddrs
            go lastMerkleM mTxsAcc newMap
        Just (Left ((MerkleBlock mHead _ _ _), mTxs)) -> do
            $(logDebug) $ pack $ unwords
                [ "Buffering merkle block"
                , encodeBlockHashLE $ headerHash mHead 
                ]
            go (Just mHead) (mTxs:mTxsAcc) aMap
        -- Done processing this batch. Reverse mTxsAcc as we have been
        -- prepending new values to it.
        _ -> return (lastMerkleM, reverse mTxsAcc, aMap)
    groupByAcc addrs =
        let xs = map (\a -> (keyRingAddrAccount a, 1)) addrs
        in  M.fromListWith (+) xs
    shouldRescan aMap = do
        -- Try to find an account whos gap is smaller than the number of new
        -- addresses generated in that account.
        res <- runDBPool sem pool $ splitSelect (M.assocs aMap) $ \ks -> 
            from $ \a -> do
                let andCond (ai, cnt) = 
                        a ^. KeyRingAccountId ==. val ai &&.
                        a ^. KeyRingAccountGap <=. val cnt
                where_ $ join2 $ map andCond ks
                return $ a ^. KeyRingAccountId
        return $ not $ null res
    -- Some logging of the blocks
    logBlockChainAction action = case action of
        BestChain nodes -> $(logInfo) $ pack $ unwords
            [ "Best chain height"
            , show $ nodeHeaderHeight $ last nodes
            , "(", encodeBlockHashLE $ nodeBlockHash $ last nodes, ")"
            ]
        ChainReorg _ o n -> $(logInfo) $ pack $ unlines $
            [ "Chain reorg."
            , "Orphaned blocks:" 
            ]
            ++ map (("  " ++) . encodeBlockHashLE . nodeBlockHash) o
            ++ [ "New blocks:" ]
            ++ map (("  " ++) . encodeBlockHashLE . nodeBlockHash) n
            ++ [ unwords [ "Best merkle chain height"
                        , show $ nodeHeaderHeight $ last n
                        ]
            ]
        SideChain n -> $(logWarn) $ pack $ unlines $
            "Side chain:" : 
            map (("  " ++) . encodeBlockHashLE . nodeBlockHash) n
        
maybeDetach :: Config -> IO () -> IO ()
maybeDetach cfg action =
    if configDetach cfg then runDetached pidFile logFile action else action
  where
    pidFile = Just $ configPidFile cfg
    logFile = ToFile $ configLogFile cfg

stopSPVServer :: Config -> IO ()
stopSPVServer cfg =
    -- TODO: Should we send a message instead of killing the process ?
    killAndWait $ configPidFile cfg

-- Run the main ZeroMQ loop
-- TODO: Support concurrent requests using DEALER socket when we can do
-- concurrent MySQL requests.
runWalletApp :: ( MonadIO m
                , MonadLogger m
                , MonadBaseControl IO m
                , MonadBase IO m
                , MonadThrow m
                , MonadResource m
                ) 
             => HandlerSession -> m ()
runWalletApp session = 
    liftBaseOpDiscard withContext $ \ctx ->
        liftBaseOpDiscard (withSocket ctx Rep) $ \sock -> do
            liftIO $ bind sock $ configBind $ handlerConfig session
            forever $ do
                bs  <- liftIO $ receive sock
                res <- case decode $ toLazyBS bs of
                    Just r  -> catchErrors $ 
                        runHandler session $ dispatchRequest r
                    Nothing -> return $ ResponseError "Could not decode request"
                liftIO $ send sock [] $ toStrictBS $ encode res
  where
    catchErrors m = catches m
        [ E.Handler $ \(WalletException err) -> 
            return $ ResponseError $ pack err
        , E.Handler $ \(ErrorCall err) -> 
            return $ ResponseError $ pack err
        , E.Handler $ \(SomeException exc) -> 
            return $ ResponseError $ pack $ show exc
        ]

dispatchRequest :: ( MonadLogger m
                   , MonadBaseControl IO m
                   , MonadBase IO m
                   , MonadThrow m
                   , MonadResource m
                   , MonadIO m
                   ) 
                => WalletRequest -> Handler m (WalletResponse Value)
dispatchRequest req = liftM ResponseValid $ case req of
    GetKeyRingsR                     -> getKeyRingsR
    GetKeyRingR r                    -> getKeyRingR r
    PostKeyRingsR r                  -> postKeyRingsR r
    GetAccountsR r                   -> getAccountsR r
    PostAccountsR r na               -> postAccountsR r na
    GetAccountR r n                  -> getAccountR r n
    PostAccountKeysR r n ks          -> postAccountKeysR r n ks
    PostAccountGapR r n g            -> postAccountGapR r n g
    GetAddressesR r n t m o p        -> getAddressesR r n t m o p
    GetAddressesUnusedR r n t        -> getAddressesUnusedR r n t
    GetAddressR r n i t m o          -> getAddressR r n i t m o
    PutAddressR r n i t l            -> putAddressR r n i t l
    GetTxsR r n p                    -> getTxsR r n p
    GetAddrTxsR r n i t p            -> getAddrTxsR r n i t p
    PostTxsR r n a                   -> postTxsR r n a
    GetTxR r n h                     -> getTxR r n h
    GetOfflineTxR r n h              -> getOfflineTxR r n h
    PostOfflineTxR r n t cs          -> postOfflineTxR r n t cs
    GetBalanceR r n mc o             -> getBalanceR r n mc o
    PostNodeR na                     -> postNodeR na

