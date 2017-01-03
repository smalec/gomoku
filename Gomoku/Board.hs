module Gomoku.Board where

type Board = [[Int]]

alphabet = "ABCDEFGHIJKLMNOPQRSTUWVXYZ"

emptyBoard :: Int -> Board
emptyBoard size = [take size [0,0..] | _ <- take size [0..]]

isFullBoard :: Board -> Bool
isFullBoard [] = True
isFullBoard (x:xs) = if elem 0 x then False else isFullBoard xs

printSpaces :: Int -> IO()
printSpaces 0 = return ()
printSpaces n =
    do
        putChar ' '
        printSpaces (n-1)

printRowNumberWithSpaces :: Int -> Int -> IO()
printRowNumberWithSpaces rowNumber placeholderLen =
    do
        putStr (show rowNumber)
        printSpaces (placeholderLen - length(show rowNumber) + 1)

printRow :: [Int] -> IO()
printRow [] = putChar '\n'
printRow (y:ys) =
    do
        if y == 0 then putStr ". " else
            if y == 1 then putStr "x " else putStr "o "
        printRow ys

printBoardFooter :: Int -> Int -> IO()
printBoardFooter mx placeholderLen =
    do
        printSpaces (placeholderLen + 1)
        printBoardFooter 0 mx
        putStrLn ""
        where
            printBoardFooter :: Int -> Int -> IO()
            printBoardFooter curr mx = if curr == mx then putChar '\n' else
                do
                    putChar (alphabet!!curr)
                    putChar ' '
                    printBoardFooter (curr+1) mx

printBoard :: Board -> IO()
printBoard board = printBoard 1 (length board) board
    where
        printBoard :: Int -> Int -> Board -> IO()
        printBoard _ len [] =
            do
                putChar '\n'
                printBoardFooter len (length(show len))
        printBoard row len (x:xs) = 
            do
                printRowNumberWithSpaces row (length(show len))
                printRow x
                printBoard (row+1) len xs

updateRow :: [Int] -> Int -> Int -> [Int]
updateRow row 0 value = value : (tail row)
updateRow row col value = (head row) : (updateRow (tail row) (col-1) value)

updateBoard :: Board -> (Int, Int) -> Int -> Board
updateBoard board (row, 0) value = (updateRow (head board) row value) : (tail board)
updateBoard board (row, col) value = (head board) : (updateBoard (tail board) (row, col-1) value)
