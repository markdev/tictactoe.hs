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
    where onePlayerMode = do
             putStrLn "One playa"
             putStrLn "Cool!  Get ready to play...AGAINST MY INVINCIBLE TIC TAC TOE AI!!!!! HAHAHAHA!!!"
             gameLoop1P emptyBoard PX
          twoPlayerMode = do
             putStrLn "Two players"
             gameLoop2P emptyBoard PX
          emptyBoard = [[A,B,C],[D,E,F],[G,H,I]]

gameLoop1P = gameLoop 1
gameLoop2P = gameLoop 2
gameLoop :: Int -> Board -> Player -> IO ()
gameLoop noOfPlayers board player = do
    case detectWin board of Just XWin -> endgame board XWin
                            Just OWin -> endgame board OWin
                            Just Tie -> endgame board Tie
                            Nothing -> if noOfPlayers == 1
                                       then if player == PX 
                                            then enterMove 1 board player 
                                            else enterBestMove board PO
                                       else enterMove 2 board player

enterMove :: Int -> Board -> Player -> IO () 
enterMove noOfPlayers board player = do
     displayBoard board
     if noOfPlayers == 1
     then do putStrLn ("Make your move. (A-I)")
     else do putStrLn (show player ++ ", it's your turn. (A-I)")
     move <- getLine
     if (read (map toUpper move) :: Square) `elem` [ sq | sq <- concat board]
         then do
            gameLoop noOfPlayers (newBoard (read (map toUpper move) :: Square) player board) (if player == PX then PO else PX)
         else do
            putStrLn "That square is already occupied"
            gameLoop noOfPlayers board player

enterBestMove :: Board -> Player -> IO ()
enterBestMove board player = gameLoop1P (newBoard bestmove player board) PX
    where bestmove = fst $ findBestMove PO board
          findBestMove :: Player -> Board -> (Square, Result)
          findBestMove player board
            | player == PO = findMax results
            | player == PX = findMin results
            where findMin = foldl1 (\ acc x -> if snd x < snd acc then x else acc)
                  findMax = foldl1 (\ acc x -> if snd x > snd acc then x else acc)
                  results = [ (sq, getResult b) | (sq, b) <- boards player board ]
                  getResult b = if detectWin b == Nothing 
                                then snd (findBestMove (if player == PX then PO else PX) b) 
                                else fromJust $ detectWin b
                  boards :: Player -> Board -> [(Square, Board)]
                  boards player board = [(sq, newBoard sq player board) | sq <- concat board, sq /= X, sq /=O]

displayBoard :: Board -> IO ()
displayBoard board = do
    mapM_ print board

newBoard :: Square -> Player -> Board -> Board
newBoard move player board = [ [if sq == move then mark else sq | sq <- row] | row <- board]
    where mark = if player == PX then X else O

detectWin :: Board -> (Maybe Result)
detectWin board
   | [X,X,X] `elem` board ++ transpose board = Just XWin
   | [X,X,X] `elem` [diagonal1 board, diagonal2 board] = Just XWin
   | [O,O,O] `elem` board ++ transpose board = Just OWin
   | [O,O,O] `elem` [diagonal1 board, diagonal2 board] = Just OWin
   | [X,X,X,X,X,O,O,O,O] == (sort $ concat board) = Just Tie
   | otherwise = Nothing
   where
     diagonal1 :: Board -> [Square]
     diagonal1 bs = bs!!0!!0 : bs!!1!!1 : bs!!2!!2 : []
     diagonal2 :: Board -> [Square]
     diagonal2 bs = bs!!0!!2 : bs!!1!!1 : bs!!2!!0 : []

endgame :: Board -> Result -> IO ()
endgame board result = do
    displayBoard board
    if result `elem` [XWin, OWin]
        then 
            let player = if result == XWin then PX else PO
            in do 
                putStrLn ("The game is over, and " ++ show player ++ " wins!")
                putStrLn ((if player == PX then show PO else show PX) ++ " is a loser lol")
        else do
            putStrLn "The game is a tie"
            putStrLn "You are both losers!  Ugh!"
    putStrLn "Want to play again? (y/n)"
    again <- getLine
    if again `elem` ["y", "Y", "yes", "Yes", "YES"] 
        then gameSelect 
        else do
            putStrLn "Goodbye"



