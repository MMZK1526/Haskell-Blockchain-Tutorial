module Main where

import           Control.Monad
import           Control.Effect.Lift
import           Workflow.Mine
import           Model.BlockHeader
import           Workflow.LoadEnv

main :: IO ()
main = withEnv do
  replicateM_ 100 do
    newBlock <- mineBlock
    sendIO $ print newBlock
