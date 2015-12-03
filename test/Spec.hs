import Game.Types
import Game.Game
import Data.Matrix
import Test.Hspec
import Lib
import Control.Monad.Trans.Either
import Servant.Server.Internal.ServantErr

main :: IO ()
main = hspec $ do
    describe "giveWinner" $ do
        it "returns Both when both players have won in a board" $ do
            giveWinner m `shouldBe` (Both :: Square)

    testValidate
        where m = fromList 3 3 [X,X,X,O,O,O,O,X,O]

testValidate = do
    describe "validate" $ do
        describe "first move" $ do
            it "returns the same game if the first move is made" $ do
                nextMove <- runEitherT $ validateMove (Move "ONE" 3 3) freshGame 
                nextMove `shouldBe` pure freshGame

        describe "outerMove" $ do
            it "throws err412 'invalid outer square'" $ do
                let nextMove = Move "ONE" 3 6
                validateMove nextMove activeGame `shouldBe` err412

    where
        freshGame = newGame "ONE" "TWO"
        activeGame = Game {
            playerX = "ONE",
            playerO = "TWO",
            lastMove = Move {
                player = "TWO",
                outerPos = 1,
                innerPos = 6
            },
            board = fromList 3 3 [
                fromList 3 3 [
                    O,      O,      O,
                    X,      X,      O,
                    X,      X,      X
                ],

                fromList 3 3 [
                    X,      X,      O,
                    X,      O,  Empty,
                    X,  Empty,  Empty
                ],

                fromList 3 3 [
                    X,      X,      X,
                Empty,      O,  Empty,
                Empty,  Empty,  Empty
                ],

                fromList 3 3 [
                    O,  Empty,  Empty,
                    O,  Empty,  Empty,
                    X,      X,      O
                ],

                fromList 3 3 [
                    X,      O,  Empty,
                    X,  Empty,  Empty,
                Empty,  Empty,  Empty
                ],

                fromList 3 3 [
                    O,  Empty,  Empty,
                Empty,  Empty,  Empty,
                Empty,  Empty,  Empty
                ],

                fromList 3 3 [
                    O,  Empty,  Empty,
                    O,  Empty,      X,
                    O,  Empty,  Empty
                ],

                fromList 3 3 [
                    O,      O,  Empty,
                Empty,  Empty,  Empty,
                Empty,  Empty,  Empty
                ],

                fromList 3 3 [
                    X,      O,  Empty,
                Empty,  Empty,  Empty,
                Empty,  Empty,  Empty
                ]
            ],
            metaBoard = fromList 3 3 [
                 Both,      X,      X,
                Empty,  Empty,  Empty,
                    O,  Empty,  Empty
            ],
            moves = 36,
            gameWon = Empty
        }

