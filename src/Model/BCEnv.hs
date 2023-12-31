module Model.BCEnv where

import           Control.Effect.Lift
import           Control.Effect.State
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics
import           Model.Block
import           Model.Transaction
import           Model.Wallet

data BCEnv
  = BCEnv { blockchains :: [Block]
          , mempool :: [Transaction]
          , wallet :: Wallet }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type MonadBCEnv sig m = (Has (State BCEnv) sig m, Has (Lift IO) sig m)
