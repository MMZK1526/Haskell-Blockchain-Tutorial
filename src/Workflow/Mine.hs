module Workflow.Mine where

import           Class.BCHashable
import           Control.Effect.State
import           Data.List
import           Model.BCEnv
import           Model.Block
import           Model.BlockHeader
import           Model.Transaction
import           Model.Wallet

pickTransactions :: MonadBCEnv sig m => BlockHeader -> m [Transaction]
pickTransactions bh = do
  (pool, bad) <- partition ((<= bh.timestamp + 10) . (.lock_time))
             <$> gets mempool
  let (chosen, rest) = splitAt 100 pool
  modify (\env -> env { mempool = rest ++ bad })
  pure chosen
{-# INLINE pickTransactions #-}

nextBlockRaw :: String -> [Transaction] -> BlockHeader -> BlockHeader
nextBlockRaw miner pool bh =
  BlockHeader { height = bh.height + 1
              , timestamp = bh.timestamp + 10
              , previous_block_header_hash = bh.hash
              , transactions_merkle_root = mkMerkle pool
              , transactions_count = genericLength pool
              , miner = miner
              , nonce = 0
              , hash = zero
              , difficulty = if bh.height `mod` 50 == 0 && bh.difficulty < 6
                then bh.difficulty + 1
                else bh.difficulty
              }
{-# INLINE nextBlockRaw #-}

nextBlockMined :: BlockHeader -> BlockHeader
nextBlockMined bh
  | qed       = bh { hash = bhHash }
  | otherwise = nextBlockMined bh { nonce = bh.nonce + 1 }
  where
    qed     = take (2 + dfct) bhHash == "0x" ++ replicate dfct '0'
    bhHash  = bcHash bh
    dfct    = fromIntegral bh.difficulty
    nextRaw = bh { nonce = 0 }

mineBlockAs :: MonadBCEnv sig m => (BCEnv -> String) -> m BlockHeader
mineBlockAs selector = do
  latestBh <- (.header) . head <$> gets blockchains
  txs      <- pickTransactions latestBh
  miner    <- gets selector
  let newBh = nextBlockMined $ nextBlockRaw miner txs latestBh
  modify (\env -> env { blockchains = Block newBh txs : blockchains env })
  pure newBh
{-# INLINE mineBlockAs #-}

mineBlock :: MonadBCEnv sig m => m BlockHeader
mineBlock = mineBlockAs (pickFirstWalletAddr . wallet)
{-# INLINE mineBlock #-}
