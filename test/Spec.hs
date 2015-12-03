import Game.Types
import Data.Matrix
import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "giveWinner" $ do
        it "returns Both when both players have won in a board" $ do
            giveWinner m `shouldBe` (Both :: Square)
            where m = fromList 3 3 [X,X,X,O,O,O,O,X,O]
			    