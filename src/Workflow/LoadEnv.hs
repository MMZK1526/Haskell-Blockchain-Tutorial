{-# LANGUAGE Rank2Types #-}

module Workflow.LoadEnv where

import           Control.Carrier.Lift
import           Control.Carrier.State.Strict
import           Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import           Data.List
import           Data.Ord
import           Model.BCEnv
import           Model.Block
import           Model.Transaction
import           Model.Wallet

getBlocks :: IO [Block]
getBlocks = do
  contents <- BS.readFile "data/blockchain.json"
  let Just blocks = JSON.decodeStrict contents :: Maybe [Block]
  pure $ reverse blocks

getMemPool :: IO [Transaction]
getMemPool = do
  contents <- BS.readFile "data/mempool.json"
  let Just txs = JSON.decodeStrict contents :: Maybe [Transaction]
  pure $ sortOn (Down . transaction_fee) txs

getWallet :: IO Wallet
getWallet = do
  contents <- BS.readFile "data/keys.json"
  let Just wallet = JSON.decodeStrict contents :: Maybe Wallet
  pure wallet

getBCEnv :: IO BCEnv
getBCEnv = liftM3 BCEnv getBlocks getMemPool getWallet

withEnv :: forall a. (forall sig m. MonadBCEnv sig m => m a) -> IO a
withEnv action = do
  env <- getBCEnv
  runM $ evalState env (action :: StateC BCEnv (LiftC IO) a)
