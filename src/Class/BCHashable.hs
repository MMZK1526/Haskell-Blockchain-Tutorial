module Class.BCHashable where

import           Class.BCShow
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Text.Printf

class BCHashable a where
  bcHash :: a -> String

instance BCShow a => BCHashable a where
  bcHash :: a -> String
  bcHash = ("0x" ++) . concatMap (printf "%02x") . BS.unpack
         . SHA256.hash . C8.pack . bcShow
