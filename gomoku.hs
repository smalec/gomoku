-- Bartosz Łągwa

alphabet = "ABCDEFGHIJKLMNOPQRSTUWVXYZ"

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
        where
            printBoardFooter :: Int -> Int -> IO()
            printBoardFooter curr mx = if curr == mx then putChar '\n' else
                do
                    putChar (alphabet!!curr)
                    putChar ' '
                    printBoardFooter (curr+1) mx

printBoard :: [[Int]] -> IO()
printBoard board = printBoard 1 (length board) board
    where
        printBoard :: Int -> Int -> [[Int]] -> IO()
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

updateBoard :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateBoard board (0, col) value = (updateRow (head board) col value) : (tail board)
updateBoard board (row, col) value = (head board) : (updateBoard (tail board) (row-1, col) value) 

emptyBoard = let s = take 15 [0,0..] in [s,s,s,s,s, s,s,s,s,s, s,s,s,s,s]
updatedBoard = updateBoard (updateBoard emptyBoard (0, 0) 1) (14, 14) 2
