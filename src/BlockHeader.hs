module BlockHeader where

import           BCShow
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Word
import           GHC.Generics

data BlockHeader
  = BlockHeader { difficulty :: Word8
                , hash :: String
                , height :: Word
                , miner :: String
                , nonce :: Integer
                , previous_block_header_hash :: String
                , timestamp :: Word32
                , transactions_count :: Word8
                , transactions_merkle_root :: String }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (BCShow, FromJSON, ToJSON)
