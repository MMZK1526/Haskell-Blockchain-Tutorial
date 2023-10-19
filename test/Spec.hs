import           Control.Monad
import           Model.BCEnv
import           Model.Block
import           Model.BlockHeader
import           Model.Transaction
import           Test.Hspec
import           Workflow.LoadEnv

main :: IO ()
main = hspec do
  env <- runIO getBCEnv
  describe "Merkle Tests" do
    forM_ env.blockchains \b -> it ("Block " ++ show b.header.height) do
      mkMerkle b.transactions `shouldBe` b.header.transactions_merkle_root
