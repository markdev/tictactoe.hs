import Data.List
import Data.Char
import Data.Maybe
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
data Player = PX | PO deriving (Read, Eq)
instance Show Player where
   show PX = "Player X"
   show PO = "Player O"
data Result = XWin | Tie | OWin deriving (Read, Show, Eq, Ord) 

main :: IO ()
main = do
    putStrLn "Let's play some motherfuckin tic tac toe!!!"
    putStrLn "Yeeeaaaaaahh!!!"
    gameSelect

gameSelect :: IO ()
gameSelect = do
    putStrLn "Who gonna play, one playa or two??? (Enter 1 or 2)"
    gameMode <- getLine
    case gameMode of "1" -> onePlayerMode
                     "2" -> twoPlayerMode
                     gameMode -> gameSelect

onePlayerMode :: IO ()
onePlayerMode = do
    putStrLn "One playa"
    putStrLn "Cool!  Get ready to play...AGAINST MY INVINCIBLE TIC TAC TOE AI!!!!! HAHAHAHA!!!"
    onePlayerGameLoop emptyBoard PX

onePlayerGameLoop :: Board -> Player -> IO ()
onePlayerGameLoop boardstate player = do
    case detectWin boardstate of Just XWin -> endgame boardstate XWin
                                 Just OWin -> endgame boardstate OWin
                                 Just Tie -> endgame boardstate Tie
                                 Nothing -> if player == PX then enter1PMove boardstate player else enterBestMove boardstate PO

enter1PMove :: Board -> Player -> IO ()
enter1PMove boardstate player = do
     displayBoard boardstate
     putStrLn ("Make your move. (A-I)")
     move <- getLine
     if (read (map toUpper move) :: Square) `elem` [ sq | sq <- concat boardstate]
         then do
            onePlayerGameLoop (newBoard (read (map toUpper move) :: Square) player boardstate) (if player == PX then PO else PX)
         else do
            putStrLn "That square is already occupied"
            onePlayerGameLoop boardstate player

enterBestMove :: Board -> Player -> IO ()
enterBestMove boardstate player = onePlayerGameLoop (newBoard bestmove player boardstate) PX
    where bestmove = fst $ findBestMove PO boardstate

findBestMove :: Player -> Board -> (Square, Result)
findBestMove player board
   | player == PO = findMax results
   | player == PX = findMin results
   where findMin = foldl1 (\ acc x -> if snd x < snd acc then x else acc)
         findMax = foldl1 (\ acc x -> if snd x > snd acc then x else acc)
         --results = [(F,XWin),(G,OWin),(H,Tie)]
         results = [ (sq, getResult b) | (sq, b) <- boards player board ]
         getResult b = if detectWin b == Nothing 
                       then snd (findBestMove (if player == PX then PO else PX) b) 
                       else fromJust $ detectWin b

newBoard :: Square -> Player -> Board -> Board
newBoard move player boardstate = [ [if sq == move then mark else sq | sq <- row] | row <- boardstate]
    where mark = if player == PX then X else O

boards :: Player -> Board -> [(Square, Board)]
boards player board = [(sq, newBoard sq player board) | sq <- concat board, sq /= X, sq /=O]

detectWin :: Board -> (Maybe Result)
detectWin boardstate
   | [X,X,X] `elem` boardstate ++ transpose boardstate = Just XWin
   | [X,X,X] `elem` [nwtose boardstate, netosw boardstate] = Just XWin
   | [O,O,O] `elem` boardstate ++ transpose boardstate = Just OWin
   | [O,O,O] `elem` [nwtose boardstate, netosw boardstate] = Just OWin
   | [X,X,X,X,X,O,O,O,O] == (sort $ concat boardstate) = Just Tie
   | otherwise = Nothing
   where
     nwtose :: Board -> [Square]
     nwtose bs = bs!!0!!0 : bs!!1!!1 : bs!!2!!2 : []
     netosw :: Board -> [Square]
     netosw bs = bs!!0!!2 : bs!!1!!1 : bs!!2!!0 : []







twoPlayerMode :: IO ()
twoPlayerMode = do
    putStrLn "Two players"
    twoPlayerGameLoop emptyBoard PX -- start game loop with empty board, two players, initialize to player one

twoPlayerGameLoop :: Board -> Player -> IO ()
twoPlayerGameLoop boardstate player = do
    case detectWin boardstate of Just XWin -> endgame boardstate XWin
                                 Just OWin -> endgame boardstate OWin
                                 Just Tie -> endgame boardstate Tie 
                                 Nothing -> enterMove boardstate player

enterMove :: Board -> Player -> IO () 
enterMove boardstate player = do
     displayBoard boardstate
     putStrLn (show player ++ ", it's your turn. (A-I)")
     move <- getLine
     if (read (map toUpper move) :: Square) `elem` [ sq | sq <- concat boardstate]
         then do
            twoPlayerGameLoop (newBoard (read (map toUpper move) :: Square) player boardstate) (if player == PX then PO else PX)
         else do
            putStrLn "That square is already occupied"
            twoPlayerGameLoop boardstate player

endgame :: Board -> Result -> IO ()
endgame boardstate result = do
    displayBoard boardstate
    if result `elem` [XWin, OWin]
        then 
            let player = if result == XWin then PX else PO
            in do 
                putStrLn ("The game is over, and " ++ show player ++ " wins!")
                putStrLn "The other guy is a loser lol"
        else do
            putStrLn "The game is a tie"
            putStrLn "You are both losers!  Ugh!"
    putStrLn "Want to play again? (y/n)"
    again <- getLine
    if again `elem` ["y", "Y", "yes", "Yes", "YES"] 
        then gameSelect 
        else do
            putStrLn "Goodbye"

displayBoard :: Board -> IO ()
displayBoard boardstate = do
    mapM_ print boardstate

emptyBoard :: Board
emptyBoard = [[A,B,C],[D,E,F],[G,H,I]]



