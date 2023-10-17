module Model.Transaction where

import           Class.BCHashable
import           Class.BCShow
import           Data.Aeson (FromJSON, ToJSON)
import           Data.List
import           Data.Word
import           GHC.Generics

data Transaction
  = Transaction { amount :: Word
                , lock_time :: Word32
                , receiver :: String
                , sender :: String
                , signature :: String
                , transaction_fee :: Word }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (BCShow, FromJSON, ToJSON)

mkMerkle :: [Transaction] -> String
mkMerkle txs = go txHashes
  where
    txHashes             = bcHash <$> txs
    go [root]            = root
    go hs                = go $ worker hs
    worker []            = []
    worker [rem]         = [bcHash (zero ++ rem)]
    worker (h : h' : hs) = let [s, s'] = sort [h, h']
                           in  bcHash (s ++ s') : worker hs
