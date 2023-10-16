{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BCHashable
import           Block
import           BlockHeader
import           Transaction

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Maybe
import Data.Word
import GHC.Generics
import Text.Printf
import Data.Ord

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

getBlocks :: IO [Block]
getBlocks = do
  contents <- BS.readFile "data/blockchain.json"
  let Just blocks = JSON.decodeStrict contents :: Maybe [Block]
  pure blocks

getMemPool :: IO [Transaction]
getMemPool = do
  contents <- BS.readFile "data/mempool.json"
  let Just txs = JSON.decodeStrict contents :: Maybe [Transaction]
  pure $ sortOn (Down . transaction_fee) txs

zero :: String
zero = "0x" ++ replicate 64 '0'

mkMerkle :: [Transaction] -> String
mkMerkle txs = go txHashes
  where
    txHashes     = bcHash <$> txs
    go [root]    = root
    go hs        = go $ worker hs
    worker []    = []
    worker [rem] = [bcHash (zero ++ rem)]
    worker (h : h' : hs) = let [s, s'] = sort [h, h'] in bcHash (s ++ s') : worker hs

mine :: [Transaction] -> BlockHeader -> BlockHeader
mine txs b = go nextRaw
  where
    go rawBlock = if take (2 + d) hashValue == "0x" ++ replicate d '0'
        then rawBlock { hash = hashValue }
        else go $ rawBlock { nonce = nonce rawBlock + 1 }
      where
        hashValue = bcHash rawBlock
    txs' = filter (\tx -> lock_time tx <= newTimestamp) txs
    d :: Num a => a
    d = fromIntegral $ difficulty b
    selctedTxs = take 100 txs'
    newTimestamp = timestamp b + 10
    nextRaw = BlockHeader { timestamp = newTimestamp
                          , height = height b + 1
                          , previous_block_header_hash = hash b
                          , transactions_merkle_root = mkMerkle selctedTxs
                          , transactions_count = genericLength selctedTxs
                          , nonce = 0
                          , hash = zero
                          , miner = zero
                          , difficulty = if height nextRaw `mod` 50 == 0 && d < 6 then d + 1 else d
                          }

main :: IO ()
main = do
  pool <- getMemPool
  print $ length pool
  blocks <- getBlocks
  let Block b ts = last blocks
  print $ mine pool b
