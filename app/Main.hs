{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Random
import Data.List (nub)
import Data.Char (intToDigit)
import Control.Monad (forM_)
import Data.Foldable (Foldable(foldr'))
--import qualified MyLib (someFunc)

type Coords = (Int, Int)

data EmptySquareState = Covered | Flagged | Uncovered deriving (Show, Eq)

data Square = Empty { nearby :: Int, state :: EmptySquareState } | Mine { flagged :: Bool } deriving (Show)

displaySquare :: Square -> Char
displaySquare Empty{..} =
    case state of
        Covered -> '-'
        Flagged -> 'X'
        Uncovered -> intToDigit nearby
displaySquare Mine{..} = if flagged then 'X' else '-'

isMine :: Square -> Bool
isMine Mine{}  = True
isMine Empty{} = False

type Board = [[Square]]

getSquareAt :: Board -> Coords -> Square
getSquareAt board (x, y) = (board !! y) !! x

setSquareAt :: Board -> Coords -> Square -> Board
setSquareAt board (x, y) square =
    let (before, row:after) = splitAt y board
        newRow = take x row ++ [square] ++ drop (x + 1) row
    in before ++ [newRow] ++ after

adjacentCoords :: Board -> Coords -> [Coords]
adjacentCoords board (x, y) =
    [ (x+dx, y+dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , not $ dx == 0 && dy == 0
    , x + dx >= 0
    , y + dy >= 0
    , x + dx < length (head board) -- assumes that the board is a rectangle, i.e. the length of every row is the same
    , y + dy < length board
    ]

countAdjacentMines :: Board -> Coords -> Int
countAdjacentMines board = length . filter isMine . map (getSquareAt board) . adjacentCoords board

calculateNearbyMines :: Board -> Board
calculateNearbyMines board =
    [ [ updateSquare (x, y) | (x, square) <- zip [0..] row ]
    | (y, row) <- zip [0..] board
    ] where
        updateSquare coords =
            case getSquareAt board coords of
                Empty{..} -> Empty { nearby=countAdjacentMines board coords, state=state }
                Mine{..}  -> Mine { flagged=flagged } -- do nothing to mines

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard rows cols = replicate rows $ replicate cols Empty { nearby=0, state=Covered }

placeMines :: RandomGen g => Board -> Int -> g -> Board
placeMines board numMines gen =
    let coords = [ (x, y) | x <- [0 .. length (head board) - 1], y <- [0 .. length board - 1] ]
        mineCoords = take numMines $ nub $ randomRs (0, length coords - 1) gen
        updateBoard coordIndex b =
            let coord = coords !! coordIndex
            in setSquareAt b coord Mine{ flagged=False }
    in foldr updateBoard board mineCoords

displayBoard :: Board -> String
displayBoard board = unlines $ map handleRow board where
    handleRow = map displaySquare

main :: IO ()
main = do
    gen <- newStdGen

    putStrLn "Number of rows:"
    r <- getLine
    let rows = read r :: Int

    putStrLn "Number of cols:"
    c <- getLine
    let cols = read c :: Int

    putStrLn "Number of mines:"
    m <- getLine
    let numberOfMines = read m :: Int

    let emptyBoard = createEmptyBoard rows cols
        boardWithMines = placeMines emptyBoard numberOfMines gen
        finalBoard = calculateNearbyMines boardWithMines

    game finalBoard

game :: Board -> IO ()
game board = do
    putStrLn $ displayBoard board

    putStrLn "Enter X to mark a tile with a flag, otherwise enter anything else to attempt to open tile"
    input1 <- getLine
    let shouldFlag = input1 == "X"

    putStrLn "Enter x:"
    input2 <- getLine
    let x = read input2 :: Int

    putStrLn "Enter y:"
    input3 <- getLine
    let y = read input3 :: Int

    putStrLn "" -- empty line

    if shouldFlag then
        let square = getSquareAt board (x, y)
            newSquare =
                case square of
                    Empty{..} ->
                        if state == Covered then
                            Empty { nearby=nearby, state=Flagged }
                        else if state == Flagged then
                            Empty { nearby=nearby, state=Covered }
                        else
                            Empty { nearby=nearby, state=Uncovered} -- do nothing, player cannot flag uncovered square
                    Mine{..} -> Mine { flagged=not flagged } -- flip between flagged and unflagged
        in game $ setSquareAt board (x, y) newSquare
    else
        case getSquareAt board (x, y) of
            Empty{..} ->
                let isCovered Empty{..} = state /= Uncovered
                    isCovered Mine{} = True
                    newBoard = uncoverSquare board (x, y)
                in if all isMine $ filter isCovered $ concat newBoard then
                    putStrLn "You win!"
                else game newBoard
            Mine{} -> putStrLn "Game Over!"

uncoverSquare :: Board -> Coords -> Board
uncoverSquare board coords =
    let currentSquare = getSquareAt board coords
    in case currentSquare of
        Empty{..} ->
            if state == Covered then
                let newBoard = setSquareAt board coords Empty { nearby=nearby, state=Uncovered }
                in if nearby == 0 then
                    foldr' (flip uncoverSquare) newBoard (adjacentCoords board coords) -- apply uncoverSquare to all adjacent squares
                else
                    newBoard
            else
                board -- don't do anything to squares already uncovered
        Mine{} -> error "how did we get here?"

{-
    let Empty{..} = getSquareAt board x y -- assuming we are only uncovering empty squares in this function
        newBoard = setSquareAt board x y Empty { nearby=nearby, state=Uncovered }
    in if nearby == 0 then
        foldr' (\(x, y) b -> uncoverSquare b x y) newBoard (adjacentCoords board x y)
    else
        newBoard
-}
