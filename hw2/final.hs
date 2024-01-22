--BLG458E Functional Programming Hw2
--Malik Kalembaşı 150180112

{-# LANGUAGE LambdaCase #-}

module Chessboard where

import Data.Maybe (isNothing, isJust, catMaybes)
import Data.List (delete, maximumBy)
import Data.Ord (comparing)
import Control.Monad (when)

--PART 1: Chessboard Types
data Piece = Bishop | Knight | Rook | Queen | Pawn deriving (Show, Eq)
data Team = Red | Green | Blue | Purple | White deriving (Show, Eq)
type Position = (Int, Int) --a position on the chessboard should be a pair of coordinates
type Chessboard = [[Maybe (Piece, Team)]] --the chessboard should be able to represent empty squares

--PART 2: Piece Movements
type Coord = (Int, Int)

--generate all possible moves for each piece
bishopMoves, knightMoves, rookMoves, queenMoves, pawnMoves :: Coord -> [Coord]
bishopMoves (x, y) = [(x+i, y+i) | i <- [-7..7], i /= 0] ++ [(x-i, y+i) | i <- [-7..7], i /= 0]
knightMoves (x, y) = [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1), (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2)]
rookMoves (x, y)   = [(x+i, y) | i <- [-7..7], i /= 0] ++ [(x, y+i) | i <- [-7..7], i /= 0]
queenMoves (x, y)  = bishopMoves (x, y) ++ rookMoves (x, y)
pawnMoves (x, y)   = [(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]

--filter moves to keep only those within the board
validMoves :: (Coord -> [Coord]) -> Coord -> [Coord]
validMoves moveFunc coord = filter withinBoard (moveFunc coord)
  where withinBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

--calculate the total number of attacks on the board
attackCount :: Chessboard -> Int
attackCount board = sum [ countAttacks (piece, team) position board |
                          (rowIndex, row) <- zip [0..] board,
                          (colIndex, cell) <- zip [0..] row,
                          let position = (rowIndex, colIndex),
                          Just (piece, team) <- [cell] ]
                          
--helper function to count attacks for a specific piece on the board.
countAttacks :: (Piece, Team) -> Position -> Chessboard -> Int
countAttacks attacker position board = length $ filter (canAttacked attacker position board) allPositions
  where
    allPositions = [(rIndex, cIndex) | (rIndex, row) <- zip [0..] board, (cIndex, Just _) <- zip [0..] row]

--helper function to determine if a piece can attack another piece.
canAttacked :: (Piece, Team) -> Position -> Chessboard -> Position -> Bool
canAttacked (attackerType, attackerTeam) attackerPos board defenderPos
    | isSameTeam = False  -- Cannot attack same team
    | otherwise = canAttack attackerType attackerPos (Just defenderPos) board
  where
    isSameTeam = maybe False (\(_, team) -> attackerTeam == team) (getPieceAt defenderPos board)

--determines if a piece can attack a position on the board based on the piece type.
canAttack :: Piece -> Position -> Maybe Position -> Chessboard -> Bool
canAttack piece posAttacker posDefender board =
  case piece of
    Pawn   -> pawnAttack posAttacker posDefender
    Knight -> knightAttack posAttacker posDefender
    Bishop -> bishopAttack posAttacker posDefender board
    Rook   -> rookAttack posAttacker posDefender board
    Queen  -> queenAttack posAttacker posDefender board

--determines if a pawn can attack a position, considering all diagonal directions.
pawnAttack :: Position -> Maybe Position -> Bool
pawnAttack (x, y) mPos = maybe False (\(dx, dy) -> abs (dx - x) == 1 && abs (dy - y) == 1) mPos

--determines if a knight can attack a position, considering its L-shaped movement.
knightAttack :: Position -> Maybe Position -> Bool
knightAttack (x, y) mPos = maybe False (\(dx, dy) -> (abs (x - dx), abs (y - dy)) `elem` [(2, 1), (1, 2)]) mPos

--checks if the attack is diagonal based on the positions of the attacker and defender.
diagonalAttack :: Position -> Position -> Bool
diagonalAttack (x1, y1) (x2, y2) = abs (x2 - x1) == abs (y2 - y1)

--determines if a bishop can attack a position, considering diagonal movements and path clearance.
bishopAttack :: Position -> Maybe Position -> Chessboard -> Bool
bishopAttack posAttacker mPosDefender board = maybe False (\posDefender -> diagonalAttack posAttacker posDefender && isPathClear (generatePath posAttacker posDefender) board) mPosDefender

--dhecks if the attack is straight (either horizontal or vertical) based on the positions.
straightAttack :: Position -> Position -> Bool
straightAttack (x1, y1) (x2, y2) = x1 == x2 || y1 == y2

--determines if a rook can attack a position, considering straight movements and path clearance.
rookAttack :: Position -> Maybe Position -> Chessboard -> Bool
rookAttack posAttacker mPosDefender board = maybe False (\posDefender -> straightAttack posAttacker posDefender && isPathClear (generatePath posAttacker posDefender) board) mPosDefender

--determines if a queen can attack a position, considering both straight and diagonal movements.
queenAttack :: Position -> Maybe Position -> Chessboard -> Bool
queenAttack posAttacker posDefender board =
  bishopAttack posAttacker posDefender board || rookAttack posAttacker posDefender board

--generates a list of integers from one number to another.
range :: Int -> Int -> [Int]
range a b = [a, a + signum (b - a) .. b]

--generates the path between two positions on the board.
generatePath :: Position -> Position -> [Position]
generatePath (x1, y1) (x2, y2) =
  [(x, y) | x <- range x1 x2, y <- range y1 y2, x == x1 || y == y1 || abs (x - x1) == abs (y - y1)]

--checks if the path between two positions on the board is clear of other pieces.
isPathClear :: [Position] -> Chessboard -> Bool
isPathClear path board = all (isNothing . flip getPieceAt board) path

--get the piece at a specific position on the board
getPieceAt :: Position -> Chessboard -> Maybe (Piece, Team)
getPieceAt (x, y) board
    | inBounds (x, y) = (board !! x) !! y
    | otherwise = Nothing

--check if a position is within the bounds of the board
inBounds :: Position -> Bool
inBounds (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

--calculate the total value of the board
boardValue :: Chessboard -> Double
boardValue = sum . map pieceValue . catMaybes . concat

--swap upper and lower parts of the board horizontally
horizontalCrossover :: Chessboard -> Chessboard
horizontalCrossover board = let (top, bottom) = splitAt (length board `div` 2) board
                            in reverse bottom ++ top

--swap left and right parts of the board vertically
verticalCrossover :: Chessboard -> Chessboard
verticalCrossover = map reverse

--delete a piece from the board
deletePiece :: Position -> Chessboard -> Chessboard
deletePiece (x, y) board =
    take x board ++ [updateRow (board !! x)] ++ drop (x + 1) board
    where updateRow row = take y row ++ [Nothing] ++ drop (y + 1) row

--helper function to get the piece value
pieceValue :: (Piece, Team) -> Double
pieceValue (Bishop, _) = 3.25
pieceValue (Knight, _) = 3.25
pieceValue (Rook, _)   = 5
pieceValue (Queen, _)  = 10
pieceValue (Pawn, _)   = 1

--PART 3: Chessboard Tree
data ChessboardTree = Node Chessboard [ChessboardTree]
    deriving (Show, Eq)

--function to create a tree of chessboards
createChessboardTree :: Chessboard -> Int -> ChessboardTree
createChessboardTree board depth
  | depth <= 0 = Node board []
  | otherwise  = Node board branches
  where
    horizontalBranch = horizontalCrossover board
    verticalBranch = verticalCrossover board
    deleteBranches = [deletePiece (x, y) board | x <- [0..7], y <- [0..7], isJust ((board !! x) !! y)]
    allBranches = horizontalBranch : verticalBranch : deleteBranches
    branches = concatMap (\b -> [createChessboardTree b (depth - 1)]) allBranches

--heuristic to evaluate how good a chessboard configuration is
evaluateChessboard :: Chessboard -> Double
evaluateChessboard board = boardValue board - fromIntegral (attackCount board)

--function to find the best chessboard at each level of the tree
findBestChessboards :: ChessboardTree -> [Chessboard]
findBestChessboards (Node board []) = [board]
findBestChessboards (Node _ children) =
  map (maximumBy (comparing evaluateChessboard) . findBestChessboards) children

--starting board configuration
startBoard :: Chessboard
startBoard =
  [ [Nothing, Nothing, Nothing, Just (Bishop, White), Just (Knight, Purple), Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Just (Queen, Purple), Nothing, Nothing, Nothing, Just (Knight, Purple), Nothing]
  , [Nothing, Nothing, Nothing, Just (Knight, White), Nothing, Just (Rook, Green), Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Just (Queen, Blue), Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Knight, Blue), Nothing]
  , [Nothing, Nothing, Just (Queen, Red), Nothing, Just (Rook, Green), Just (Bishop, White), Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Just (Bishop, White), Nothing, Nothing]
  ]

--TESTING
data ChessboardNode = ChessboardNode Chessboard Int deriving (Show, Eq)

--this function finds the best chessboard configuration at each level of the tree up to a specified maximum depth
--it takes a ChessboardNode (which contains a chessboard and its current depth) and the maximum depth as input
--the function returns a list of tuples, where each tuple contains the depth and the best chessboard configuration at that level
--if the current depth reaches the maximum depth, it returns the current board
--otherwise, it recursively explores child nodes (next possible chessboard states) to find the best configuration
findBestBoardAtEachLevel :: ChessboardNode -> Int -> [(Int, Chessboard)]
findBestBoardAtEachLevel (ChessboardNode board depth) maxDepth
  | depth >= maxDepth = [(depth, board)]
  | otherwise = (depth, board) : concatMap (`findBestBoardAtEachLevel` maxDepth) (childNodes board depth)
  where
    childNodes b d = if d < maxDepth 
                     then map (\newBoard -> ChessboardNode newBoard (d + 1)) $ nextBoards b
                     else []

--generate the next possible boards from a given board
nextBoards :: Chessboard -> [Chessboard]
nextBoards board = horizontalCrossover board : verticalCrossover board : deletePieceBoards board

--generate all possible boards by deleting pieces
deletePieceBoards :: Chessboard -> [Chessboard]
deletePieceBoards board = [deletePiece (x, y) board | x <- [0..7], y <- [0..7], isJust ((board !! x) !! y)]

--calculate the total value of a team on the board
teamValue :: Team -> Chessboard -> Double
teamValue team = sum . map pieceValue . catMaybes . concatMap (filterPieces team)
  where
    filterPieces t = filter (\case
                                Just (_, team') -> t == team'
                                Nothing -> False)

--print the details of a chessboard at a given level
printLevel :: Int -> Chessboard -> IO ()
printLevel level board = do
    let attacks = attackCount board
    let teamValues = map (\team -> (team, teamValue team board)) [Red, Green, Blue, Purple, White]

    putStrLn $ "Level: " ++ show level
    putStrLn $ "Total Number of Attacks: " ++ show attacks
    putStrLn "Team Values: "
    mapM_ (\(team, value) -> putStrLn $ "-" ++ show team ++ ": " ++ show value) teamValues
    putStrLn ""

main :: IO ()
main = do
    let rootNode = ChessboardNode startBoard 0
    let maxDepth = 5  --max depth
    let bestBoards = findBestBoardAtEachLevel rootNode maxDepth
    putStrLn "Best Boards at Each Level with Metrics:"
    mapM_ (\(level, board) -> when (level <= maxDepth) (printLevel level board)) bestBoards