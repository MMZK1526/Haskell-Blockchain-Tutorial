module Class.BCShow where

import           Control.Monad
import           Data.Int
import           Data.List
import           Data.Word
import           GHC.Generics

instance BCShow Int where
  bcShow :: Int -> String
  bcShow = show

instance BCShow Int8 where
  bcShow :: Int8 -> String
  bcShow = show

instance BCShow Int16 where
  bcShow :: Int16 -> String
  bcShow = show

instance BCShow Int32 where
  bcShow :: Int32 -> String
  bcShow = show

instance BCShow Int64 where
  bcShow :: Int64 -> String
  bcShow = show

instance BCShow Integer where
  bcShow :: Integer -> String
  bcShow = show

instance BCShow Word where
  bcShow :: Word -> String
  bcShow = show

instance BCShow Word8 where
  bcShow :: Word8 -> String
  bcShow = show

instance BCShow Word16 where
  bcShow :: Word16 -> String
  bcShow = show

instance BCShow Word32 where
  bcShow :: Word32 -> String
  bcShow = show

instance BCShow Word64 where
  bcShow :: Word64 -> String
  bcShow = show

instance BCShow Float where
  bcShow :: Float -> String
  bcShow = show

instance BCShow Double where
  bcShow :: Double -> String
  bcShow = show

instance BCShow Bool where
  bcShow :: Bool -> String
  bcShow = show

instance BCShow Char where
  bcShow :: Char -> String
  bcShow = show

instance BCShow String where
  bcShow :: String -> String
  bcShow = id

class BCShow a where
  bcShow :: a -> String
  default bcShow :: (Generic a, GBCShow (Rep a)) => a -> String
  bcShow = gbcShow . from

class GBCShow rep where
  gbcShowList :: rep a -> [(String, String)]

  gbcShow :: rep a -> String
  gbcShow a = intercalate "," (snd <$> sort (gbcShowList a))

instance BCShow a => GBCShow (K1 i a) where
  gbcShowList :: K1 i a p -> [(String, String)]
  gbcShowList (K1 a) = [("", bcShow a)]

instance (GBCShow a, Selector s) => GBCShow (S1 s a) where
  gbcShowList :: S1 s a p -> [(String, String)]
  gbcShowList m@(M1 a) = (selName m, gbcShow a) <$ guard (selName m /= "hash")

instance GBCShow a => GBCShow (C1 s a) where
  gbcShowList :: C1 s a p -> [(String, String)]
  gbcShowList (M1 a) = gbcShowList a

instance GBCShow a => GBCShow (D1 s a) where
  gbcShowList :: D1 s a p -> [(String, String)]
  gbcShowList (M1 a) = gbcShowList a

instance (GBCShow a, GBCShow b) => GBCShow (a :*: b) where
  gbcShowList :: (a :*: b) p -> [(String, String)]
  gbcShowList (a :*: b) = gbcShowList a ++ gbcShowList b
