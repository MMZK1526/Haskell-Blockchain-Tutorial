module Model.Wallet where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map (Map)
import qualified Data.Map as M
import           GHC.Generics

newtype Wallet = Wallet (Map String String)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
