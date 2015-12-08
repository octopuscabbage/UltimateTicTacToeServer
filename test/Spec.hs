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
            let m = [X,X,X,O,O,O,O,X,O]
            giveWinner m `shouldBe` (Both :: Square)

    testValidate

testValidate = do
    describe "validate" $ do
        describe "outerMove" $ do
            it "throws err403 'Forbidden' if next outerMove is not equal to last innerMove" $ do
                let nextMove = Move "ONE" 3 6
                result <- runEitherT $ validateMove nextMove activeGame
                expected <- runEitherT $ left err403 
                result `shouldBe` expected

        describe "currentPlayer" $ do
            it "throws err401 'Unauthorized' if currentPlayer is equal to lastPlayer" $ do
                let nextMove = Move "TWO" 3 6
                result <- runEitherT $ validateMove nextMove activeGame
                expected <- runEitherT $ left err401
                result `shouldBe` expected

        describe "noEmptyCells" $ do
            it "throws err404 'Not Found' if there are no empty cells in target outerMove" $ do
                let nextMove = Move "ONE" 1 3
                result <- runEitherT $ validateMove nextMove activeGame
                expected <- runEitherT $ left err404
                result `shouldBe` expected

        describe "occupied" $ do
            it "throws err409 'Conflict' if target innerMove is occupied (not Empty)" $ do
                let correctGame = changeLastMove activeGame (Move "TWO" 1 2)
                let nextMove = Move "ONE" 2 2
                result <- runEitherT $ validateMove nextMove correctGame
                expected <- runEitherT $ left err409
                result `shouldBe` expected

        describe "gameWon" $ do
            it "throws err410 'Gone' if activeGame is won (gameWon not equal to Empty)" $ do
                let correctGame = winGame activeGame X
                let nextMove = Move "ONE" 4 2
                result <- runEitherT $ validateMove nextMove correctGame
                expected <- runEitherT $ left err410
                result `shouldBe` expected

        describe "first move" $ do
            it "returns the given gameState if the first move is made (lastPlayer = 'None')" $ do
                let freshGame = newGame "ONE" "TWO"
                let firstMove = Move "ONE" 3 3
                result <- runEitherT $ validateMove firstMove freshGame 
                result `shouldBe` pure freshGame

        describe "valid move" $ do
            it "returns the given gameState if it is a validMove" $ do
                let correctGame = changeLastMove activeGame (Move "TWO" 2 5)
                let nextMove = Move "ONE" 5 7
                result <- runEitherT $ validateMove nextMove correctGame
                result `shouldBe` pure correctGame
    where
        changeLastMove g m = g { lastMove = m }
        winGame g p = g { gameWon = p }
        activeGame = Game {
            playerX = "ONE",
            playerO = "TWO",
            lastMove = Move {
                player = "TWO",
                outerPos = 1,
                innerPos = 1
            },
            board = [
                [
                    O,      O,      O,
                    X,      X,      O,
                    X,      X,      X
                ],

                [
                    X,      X,      O,
                    X,      O,  Empty,
                    X,  Empty,  Empty
                ],

                [
                    X,      X,      X,
                Empty,      O,  Empty,
                Empty,  Empty,  Empty
                ],

                [
                    O,  Empty,  Empty,
                    O,  Empty,  Empty,
                    X,      X,      O
                ],

                [
                    X,      O,  Empty,
                    X,  Empty,  Empty,
                Empty,  Empty,  Empty
                ],

                [
                    O,  Empty,  Empty,
                Empty,  Empty,  Empty,
                Empty,  Empty,  Empty
                ],

                [
                    O,  Empty,  Empty,
                    O,  Empty,      X,
                    O,  Empty,  Empty
                ],

                [
                    O,      O,  Empty,
                Empty,  Empty,  Empty,
                Empty,  Empty,  Empty
                ],

                [
                    X,      O,  Empty,
                Empty,  Empty,  Empty,
                Empty,  Empty,  Empty
                ]
            ],
            metaBoard = [
                 Both,      X,      X,
                Empty,  Empty,  Empty,
                    O,  Empty,  Empty
            ],
            moves = 36,
            gameWon = Empty
        }

