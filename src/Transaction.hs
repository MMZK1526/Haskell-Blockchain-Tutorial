module Transaction where

import           BCShow
import           Data.Aeson (FromJSON, ToJSON)
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
