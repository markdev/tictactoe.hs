import Data.List
import Control.Monad

data Square = A | B | C | D | E | F | G | H | I | X | O deriving (Read, Eq, Ord)
instance Show Square where
	show A = "a" 
	show B = "b" 
	show C = "c" 
	show D = "d" 
	show E = "e" 
	show F = "f" 
	show G = "g" 
	show H = "h" 
	show I = "i" 
	show X = "X" 
	show O = "O" 
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
	case detectWin boardstate of "1" -> endgame boardstate "1"
	                             "2" -> endgame boardstate "2"
	                             "0" -> enterMove boardstate players currentPlayer

enterMove boardstate players currentPlayer
    | players == "2" = do
        displayBoard boardstate
        putStrLn ("Player " ++ currentPlayer ++ ", it's your turn. (A-I)")
        move <- getLine
        if move `elem` [ show sq | sq <- concat boardstate]
            then do
           	   gameLoop (newBoard move currentPlayer boardstate) players (if currentPlayer == "1" then "2" else "1")
            else do
           	   putStrLn "Gotta have an else"
    | otherwise = do
        putStrLn "This is the otherwise case"

endgame boardstate player = do
    displayBoard boardstate
    putStrLn ("The game is over, and player " ++ show player ++ " wins!")
    putStrLn "The other guy is a loser lol"
    putStrLn "Want to play again? (y/n)"
    again <- getLine
    if again `elem` ["y", "Y", "yes", "Yes", "YES"] 
    	then gameSelect 
    	else do
    	    putStrLn "Goodbye"

displayBoard boardstate = do
    mapM_ print boardstate

detectWin :: Board -> String
detectWin boardstate
   | [X,X,X] `elem` boardstate ++ transpose boardstate = "1"
   | [X,X,X] == nwtose boardstate ++ netosw boardstate = "1"
   | [O,O,O] `elem` boardstate ++ transpose boardstate = "2"
   | [O,O,O] == nwtose boardstate ++ netosw boardstate = "2"
   | otherwise = "0"
   where
     nwtose :: Board -> [Square]
     nwtose bs = bs!!0!!0 : bs!!1!!1 : bs!!2!!2 : []
     netosw :: Board -> [Square]
     netosw bs = bs!!0!!2 : bs!!1!!1 : bs!!2!!0 : []

emptyBoard :: Board
emptyBoard = [[A,B,C],[D,E,F],[G,H,I]]

newBoard :: String -> String -> Board -> Board
newBoard move currentPlayer boardstate = [ [if show sq == move then mark else sq | sq <- row] | row <- boardstate]
    where mark = if currentPlayer == "1" then X else O
    


