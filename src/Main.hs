module Main where

import           Control.Effect.Lift
import           Workflow.Mine
import           Model.BlockHeader
import           Workflow.LoadEnv

testBlock :: BlockHeader
testBlock
  = BlockHeader { difficulty = 5
                , height = 203
                , miner = "0xdc45038aee5144bbfa641912eaf32ebf2bad2bd7"
                , nonce = 0
                , previous_block_header_hash = "0xb2448304889df2935277464e90a73e53b9d2c5820c48de4a40d4fa5b844c7b57"
                , timestamp = 1697412660
                , transactions_count = 97
                , transactions_merkle_root = "0xddba0c2d7d38a9bc8ba357d1fcb4a4be339ab5fddf8cdcc4419970e4746d1f6e"
                , hash = "0x073c348de2486c616699fcd8267dc895f2d8b43355b126295da92df2961f8a87" }

main :: IO ()
main = withEnv do
  newBlock <- mineBlock
  sendIO $ print newBlock
