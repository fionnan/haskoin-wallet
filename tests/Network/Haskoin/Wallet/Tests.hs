module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.Binary as Bin (encode, decode)

import Network.Haskoin.Wallet.Arbitrary ()
import Network.Haskoin.Wallet

tests :: [Test]
tests =
    [ testGroup "Serialize & de-serialize types to JSON"
        [ testProperty "AccountType" (metaID :: AccountType -> Bool)
        , testProperty "TxAction" (metaID :: TxAction -> Bool)
        , testProperty "NodeAction" (metaID :: NodeAction -> Bool)
        ]
    , testGroup "Serialize & de-serialize wallet metadata"
        [ testProperty "AccountHeader" (binaryID :: AccountHeader -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

binaryID :: Binary a => a -> Bool
binaryID x = (Bin.decode . Bin.encode) x == x
