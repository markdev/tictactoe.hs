import Control.Monad

main = do
    putStrLn "Let's play some motherfuckin tic tac toe!!!"
    putStrLn "Yeeeaaaaaahh!!!"
    gameSelect

gameSelect = do
    putStrLn "Who gonna play, one playa or two??? (Enter 1 or 2)"
    gameMode <- getLine
    case gameMode of "1" -> onePlayerMode
                     "2" -> twoPlayerMode
                     gameMode -> gameSelect

onePlayerMode = do
    putStrLn "One playa"
    putStrLn "Cool!  Get ready to play...AGAINST MY INVINCIBLE TIC TAC TOE AI!!!!! HAHAHAHA!!!"
    onePlayerGameLoop "Coming soon..."

onePlayerGameLoop boardstate = do
    putStrLn boardstate

    
twoPlayerMode = do
    putStrLn "Two players"
    gameLoop (emptyBoard) 2 1 -- start game loop with empty board, two players, initialize to player one

gameLoop boardstate players currentPlayer = do
	putStrLn $ show boardstate ++ show players ++ show currentPlayer


data Square = A | B | C | D | E | F | G | H | I | X | O deriving (Show, Read)
type Row = [Square]
type Board = [Row]

emptyBoard :: Board
emptyBoard = [[A,B,C],[D,E,F],[G,H,I]]
    


