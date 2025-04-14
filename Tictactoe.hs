module Main where

import Data.List (intercalate)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Monad (when)

data Player = X | O deriving (Eq, Show)
type Board = [Maybe Player]
type Score = (Int, Int)

data GameState = GameState {
    board :: Board,
    currentPlayer :: Player,
    score :: Score,
    playerXName :: String,
    playerOName :: String
}

initialBoard :: Board
initialBoard = replicate 9 Nothing

initialScore :: Score
initialScore = (0, 0)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

updateBoard :: Board -> Int -> Player -> Board
updateBoard board pos player =
  let index = pos - 1
      (before, _:rest) = splitAt index board
  in before ++ [Just player] ++ rest

printBoard :: Board -> IO ()
printBoard board = do
  let cell n = maybe (show n) show (board !! (n-1))
  putStrLn $ "\n " ++ intercalate " | " (map cell [1..3])
  putStrLn "-----------"
  putStrLn $ " " ++ intercalate " | " (map cell [4..6])
  putStrLn "-----------"
  putStrLn $ " " ++ intercalate " | " (map cell [7..9])

winningCombinations :: [[Int]]
winningCombinations =
  [ [0,1,2], [3,4,5], [6,7,8],
    [0,3,6], [1,4,7], [2,5,8],
    [0,4,8], [2,4,6]
  ]

checkWinner :: Board -> Player -> Bool
checkWinner board player =
  any (all (\i -> board !! i == Just player)) winningCombinations

getPlayerName :: Player -> IO String
getPlayerName player = do
  putStr $ "Enter name for Player " ++ show player ++ " (leave blank for default): "
  name <- getLine
  return $ if null name 
           then if player == X then "Player 1" else "Player 2"
           else name

showScore :: GameState -> IO ()
showScore state = do
  let (xScore, oScore) = score state
  putStrLn $ "\nScore: " ++ playerXName state ++ " (X): " ++ show xScore ++ 
             " | " ++ playerOName state ++ " (O): " ++ show oScore

gameLoop :: GameState -> IO ()
gameLoop state = do
  let brd = board state
  let player = currentPlayer state
  let (xScore, oScore) = score state
  
  printBoard brd
  showScore state
  
  if checkWinner brd X then do
    putStrLn $ "\n" ++ playerXName state ++ " (X) won this round!"
    gameLoop state { board = initialBoard, 
                    score = (xScore + 1, oScore) }
    
  else if checkWinner brd O then do
    putStrLn $ "\n" ++ playerOName state ++ " (O) won this round!"
    gameLoop state { board = initialBoard, 
                    score = (xScore, oScore + 1) }
  
  else do
    let playerName = if player == X then playerXName state else playerOName state
    putStr $ "\n" ++ playerName ++ " (" ++ show player ++ "), enter position (1-9): "
    input <- getLine
    
    case reads input of
      [(pos, "")] | pos >= 1 && pos <= 9 ->
        gameLoop state { board = updateBoard brd pos player, 
                        currentPlayer = nextPlayer player }
      _ -> do
        putStrLn "Invalid input. Please enter a number between 1 and 9."
        gameLoop state

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "INFINITE TIC-TAC-TOE WITH SCORES"
  putStrLn "--------------------------------"
  
  xName <- getPlayerName X
  oName <- getPlayerName O
  
  putStrLn $ "\nGame starts! " ++ xName ++ " (X) vs " ++ oName ++ " (O)"
  
  gameLoop GameState {
    board = initialBoard,
    currentPlayer = X,
    score = initialScore,
    playerXName = xName,
    playerOName = oName
  }