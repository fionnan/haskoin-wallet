module Network.Haskoin.Wallet.Metadata
( AccountHeader
, accountHeader
) where

import           Control.Applicative    ((<$>))
import           Control.Monad          (guard, replicateM)
import           Data.Binary            (Binary, get, getWord8, put, putWord8)
import           Data.Bits              (shift, (.&.), (.|.))
import qualified Data.ByteString        as B (length, pack, unpack)
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Word              (Word8)
import           Network.Haskoin.Crypto (XPubKey)

-- | Account header. Use accountHeader to create.
data AccountHeader
  = AccountHeader
    { requiredSignatures :: Int
    , totalKeys          :: Int
    , accountLabel       :: Text
    }
  deriving (Eq, Show)

instance Binary AccountHeader where

  put (AccountHeader rs tk lb) = do
    putWord8 0x01  -- message type
    putWord8 $ fromIntegral rt  -- m of n
    putWord8 $ fromIntegral $ B.length bs  -- label length
    mapM_ putWord8 $ B.unpack bs  -- label
    putWord8 0x00  -- empty extra
   where
    rt = rs `shift` 4 .|. tk
    bs = encodeUtf8 lb

  get = do
    tm <- getWord8 >>= guard . (==0x01)  -- type
    rt <- getWord8  -- m of n
    ct <- fromIntegral <$> getWord8  -- label length
    lb <- decodeUtf8 . B.pack <$> replicateM ct getWord8  -- label
    _ <- getWord8 -- ignore extra
    let tk = fromIntegral $ rt .&. 0x0f
        rs = fromIntegral $ rt `shift` (-4)
    return $ AccountHeader rs tk lb

-- | Create an account descriptor data structure.
accountHeader
  :: Int          -- required signatures
  -> Int          -- total keys
  -> Text         -- account label
  -> AccountHeader
accountHeader rs tk lb
  | tk > 15 = error "cannot have more than 15 keys"
  | rs > tk = error "cannot have more signatures than keys"
  | B.length (encodeUtf8 lb) > 255 = error "max label size 255 characters"
  | otherwise = AccountHeader rs tk lb
