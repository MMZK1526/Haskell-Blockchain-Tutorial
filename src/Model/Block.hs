module Model.Block where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics
import           Model.BlockHeader
import           Model.Transaction

data Block
  = Block { header :: BlockHeader
          , transactions :: [Transaction] }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
