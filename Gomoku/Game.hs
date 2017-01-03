module Gomoku.Game where

import Data.List
import Data.Maybe

import Gomoku.Board
import Gomoku.Minmax

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

getIntInRange :: (Int, Int) -> IO Int
getIntInRange (min, max) =
    do  c <- getLine
        case readMaybe c of
            Just val ->
                if val >= min && val <= max then
                    return (read c :: Int)
                else retype (min, max)
            Nothing -> retype (min, max)
            where
                retype :: (Int, Int) -> IO Int
                retype (min, max) =
                    do  putStr "Niepoprawna wartosc, podaj ponownie: "
                        getIntInRange (min, max)

getIntFromAlphabetRange :: (Int, Int) -> IO Int
getIntFromAlphabetRange (min, max) =
    do  c <- getLine
        if c >= [alphabet!!min] && c <= [alphabet!!max] then
            return (fromJust $ elemIndex (c!!0) alphabet)
        else retype (min, max)
        where
            retype :: (Int, Int) -> IO Int
            retype (min, max) =
                do  putStr "Niepoprawna wartosc, podaj ponownie: "
                    getIntFromAlphabetRange (min, max)

move :: Board -> [Sequance] -> [Sequance] -> IO (Board, [Sequance], [Sequance])
move board sequances opponentSequances =
    do
        putStr "Podaj kolumne: "
        x <- getIntFromAlphabetRange (0, length board - 1)
        putStr "Podaj wiersz: "
        y <- getIntInRange (1, length board)
        let point = (x, y - 1)
        if (board!!(snd point))!!(fst point) /= 0 then
            do
                putStrLn "Pole juz zajete!"
                move board sequances opponentSequances
        else
            return (
                    updateBoard board point 1,
                    (updateSequances sequances point) ++ (newSequances point board 1),
                    updateOpponentSequances opponentSequances point
                )

computerMove :: Board -> [Sequance] -> [Sequance] -> IO (Board, [Sequance], [Sequance])
computerMove board sequances opponentSequances =
    do
        let opponentMove = minmax board sequances opponentSequances 2
        putStrLn ("Ruch komputera: " ++ [alphabet!!(fst opponentMove)] ++ (show (snd opponentMove + 1)))
        return (
                updateBoard board opponentMove 2,
                updateOpponentSequances sequances opponentMove,
                (updateSequances opponentSequances opponentMove) ++ newSequances opponentMove board 2
            )

loop :: (Bool, Board, [Sequance], [Sequance]) -> IO()
loop (False, _, _, _) = return()
loop (_, board, sequances, opponentSequances) =
    do
        printBoard board
        (board, sequances, opponentSequances) <- move board sequances opponentSequances
        printBoard board
        if win sequances then
            do
                putStrLn "Wygrales"
                loop (False, [], [], [])
        else
            if isFullBoard board then
                do
                    putStrLn "Remis"
                    loop (False, [], [], [])
            else
                do
                    (board, sequances, opponentSequances) <- computerMove board sequances opponentSequances
                    if win opponentSequances then
                        do 
                            putStrLn "Komputer wygral"
                            loop (False, [], [], [])
                    else
                        loop (True, board, sequances, opponentSequances)

gomoku :: IO()
gomoku =
    do
        let board = emptyBoard 15
        loop (True, board, [], [])
