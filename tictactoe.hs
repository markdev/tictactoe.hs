import Control.Monad

data Square = A | B | C | D | E | F | G | H | I | X | O deriving (Show, Read)
type Row = [Square]
type Board = [Row]

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
    gameLoop emptyBoard "2" "1" -- start game loop with empty board, two players, initialize to player one

gameLoop boardstate players currentPlayer = do
	putStrLn $ show boardstate ++ show players ++ show currentPlayer
	case detectWin boardstate of "1" -> endgame "1"
	                             "2" -> endgame "2"
	                             "0" -> enterMove boardstate players currentPlayer

enterMove boardstate players currentPlayer
    | players == "2" = do
        putStrLn "Inside enterMove"
        putStrLn ("Boardstate: " ++ show boardstate)
        putStrLn ("Players: " ++ players)
        putStrLn ("currentPlayer: " ++ currentPlayer)
        gameLoop boardstate players (if currentPlayer == "1" then "2" else "1")

endgame player = do
    putStrLn ("The game is over, and player " ++ show player ++ " wins!")
    putStrLn "The other guy is a loser lol"
    putStrLn "Want to play again? (y/n)"
    again <- getLine
    if again `elem` ["y", "Y", "yes", "Yes", "YES"] 
    	then gameSelect 
    	else do
    	    putStrLn "Goodbye"

detectWin boardstate = "0"


emptyBoard :: Board
emptyBoard = [[A,B,C],[D,E,F],[G,H,I]]
    


