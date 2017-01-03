module Gomoku.Minmax where

import Gomoku.Board

type Sequance = (((Int, Int), (Int, Int)), [(Int, Int)])
type Score = ((Int, Int, Int, Int), (Int, Int, Int, Int), (Int, Int, Int, Int), (Int, Int, Int, Int), (Int, Int, Int, Int))

updateSequances :: [Sequance] -> (Int, Int) -> [Sequance]
updateSequances [] _ = []
updateSequances ((ends, points):xs) point =
    if isInRange ends point 1 then
        (updateSequance (ends, points) point) ++ (updateSequances xs point)
    else
        (ends, points):(updateSequances xs point)

updateSequance :: Sequance -> (Int, Int) -> [Sequance]
updateSequance (((x1, y1), (x2, y2)), points) point =
    if point < minimum [(x1, y1), (x2, y2)] || point > maximum [(x1, y1), (x2, y2)] then
        shortenSequance (((x1, y1), (x2, y2)), points) point 2
    else
        updateSequance (((x1, y1), (x2, y2)), points) points point
    where
        updateSequance :: Sequance -> [(Int, Int)] -> (Int, Int) -> [Sequance]
        updateSequance (ends, points) [] (x,y) = 
            if (x,y) > maximum points || (x,y) < minimum points then
                (newEnds ends (x,y), (x,y):points):(shortenSequance (ends, points) (x, y) 2)
            else
                [(ends, (x,y):points)]
        updateSequance (ends, points) ((x1,y1):xs) (x,y) = if abs(x-x1) == 1 || abs(y-y1) == 1 then
            [(newEnds ends (x,y), (x,y):points)]
        else 
            updateSequance (ends, points) xs (x,y)

newEnds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> ((Int, Int), (Int, Int))
newEnds ends (x,y) = (
        maximum [(x - 4 * signum (x - fst (fst ends)), y - 4 * signum (y - snd (fst ends))), fst ends],
        minimum [(x - 4 * signum (x - fst (snd ends)), y - 4 * signum (y - snd (snd ends))), snd ends]
    )

shortenSequance :: Sequance -> (Int, Int) -> Int -> [Sequance]
shortenSequance (_, []) _ _ = []
shortenSequance (((x1, y1), (x2, y2)), points) (x, y) toShort =
    if (x, y) < head points then
        if maximum [abs ((x + toShort * signum (fst (head points)-x)) - x2), abs ((y + toShort * signum (snd (head points)-y)) - y2)] >= 4 then
            [(((x + toShort * signum (fst (head points)-x), y + toShort * signum (snd (head points)-y)), (x2, y2)), points)]
        else
            []
    else
        if maximum [abs ((x + toShort * signum (fst (head points)-x)) - x1), abs ((y + toShort * signum (snd (head points)-y)) - y1)] >= 4 then
            [(((x1, y1), (x + toShort * signum (fst (head points)-x), y + toShort * signum (snd (head points)-y))), points)]
        else
            []

updateOpponentSequances :: [Sequance] -> (Int, Int) -> [Sequance]
updateOpponentSequances [] _ = []
updateOpponentSequances ((ends, points):xs) point =
    if isInRange ends point 0 then
        (updateOpponentSequance (ends, points) point) ++ (updateOpponentSequances xs point)
    else
        (ends, points):(updateOpponentSequances xs point)

updateOpponentSequance :: Sequance -> (Int, Int) -> [Sequance]
updateOpponentSequance (ends, points) point =
    shortenSequance (ends, smaller) point 1 ++ shortenSequance (ends, greater) point 1
        where (smaller, greater) = splitPoints points point

splitPoints :: [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
splitPoints points splitPoint = splitPoints points splitPoint ([], [])
    where
        splitPoints :: [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
        splitPoints [] _ splitted = splitted
        splitPoints (point:xs) splitPoint (smaller, greater) =
            if point < splitPoint then
                splitPoints xs splitPoint (point:smaller, greater)
            else
                splitPoints xs splitPoint (smaller, point:greater)

newSequances :: (Int, Int) -> Board -> Int -> [Sequance]
newSequances (x,y) board player =
    filterTooShortSequances [
        ((newSequanceEnd (x,y) board player (-i,-j), newSequanceEnd (x,y) board player (i,j)), [(x, y)])
        | (i,j) <- [(1,1), (1,0), (0,1),(1,-1)]
    ]
    where
        newSequanceEnd :: (Int, Int) -> Board -> Int -> (Int, Int) -> (Int, Int)
        newSequanceEnd point board player direction = newSequanceEnd point board player direction point
            where
                newSequanceEnd :: (Int, Int) -> Board -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
                newSequanceEnd (x,y) board player (i,j) (x1,y1) =
                    if isOnBoardSize (x1+i, y1+j) (length board) then
                        if (board!!(y1+j))!!(x1+i) == 0 then
                            if (x1+i, y1+j) == (x+4*i, y+4*j) then
                                (x1+i, y1+j)
                            else
                                newSequanceEnd (x,y) board player (i,j) (x1+i, y1+j)
                        else
                            if (board!!(y1+j))!!(x1+i) == player then
                                (x1-i, y1-j)
                            else
                                (x1,y1)
                    else
                        (x1,y1)
        filterTooShortSequances :: [Sequance] -> [Sequance]
        filterTooShortSequances [] = []
        filterTooShortSequances ((((x1,y1), (x2,y2)), points):xs) =
            if maximum [abs (x2-x1), abs (y2-y1)] < 4 then
                filterTooShortSequances xs
            else
                (((x1,y1), (x2,y2)), points):(filterTooShortSequances xs)

isOnBoardSize :: (Int, Int) -> Int -> Bool
isOnBoardSize (x, y) size = x >= 0 && x < size && y >= 0 && y < size

isInRange :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Int -> Bool
isInRange ((x1, y1), (x2, y2)) (x, y) offset = if x < x1 - offset then False else
    if x2 == x1 then
        x == x1 && y >= y1 - offset && y <= y2 + offset
    else
        (x2-x) * signum (y2-y1) + y == y2 && x <= x2 + offset

minmax :: Board -> [Sequance] -> [Sequance] -> Int -> (Int, Int)
minmax board sequances opponentSequances step =
    snd $ maximum [
        (minmax (updateBoard board move 2) (updateOpponentSequances sequances move) ((updateSequances opponentSequances move) ++ (newSequances move board 2)) (step-1), move)
        | move <- nextMoves board 1
    ]
        where
            minmax :: Board -> [Sequance] -> [Sequance] -> Int -> Score
            minmax _ sequances opponentSequances 0 = scoreSequances sequances opponentSequances
            minmax board sequances opponentSequances step =
                if step `mod` 2 == 0 then
                    maximum [
                        minmax (updateBoard board move 2) (updateOpponentSequances sequances move) ((updateSequances opponentSequances move)++(newSequances move board 2)) (step-1)
                        | move <- nextMoves board 1
                    ]
                else
                    minimum [
                        minmax (updateBoard board move 1) ((updateSequances sequances move)++(newSequances move board 1)) (updateOpponentSequances opponentSequances move) (step-1)
                        | move <- nextMoves board 1
                    ]

sequanceLength :: Sequance -> Int
sequanceLength (_, points) = length points

openSequance :: Sequance -> Bool
openSequance (ends, points) = not (elem (fst ends) points || elem (snd ends) points)

win :: [Sequance] -> Bool
win [] = False
win (sequance:xs) = if sequanceLength sequance >= 5 then True else win xs

updateScores :: [(Int, Int, Int, Int)] -> Int -> Bool -> Int -> [(Int, Int, Int, Int)]
updateScores (x:xs) 0 open player = (updateScore x open player):xs
    where
        updateScore :: (Int, Int, Int, Int) -> Bool -> Int -> (Int, Int, Int, Int)
        updateScore (openPlayer2, openPlayer1, player2, player1) open player =
            if open then
                if player == 1 then
                    (openPlayer2, openPlayer1-1, player2, player1)
                else
                    (openPlayer2+1, openPlayer1, player2, player1)
            else
                if player == 1 then
                    (openPlayer2, openPlayer1, player2, player1-1)
                else
                    (openPlayer2, openPlayer1, player2+1, player1)
updateScores (x:xs) row open player = x:(updateScores xs (row-1) open player)

scoreSequances :: [Sequance] -> [Sequance] -> Score
scoreSequances sequances opponentSequances = scoreSequances sequances opponentSequances [(0,0,0,0) | _ <- take 5 [0..]]
    where
        scoreSequances :: [Sequance] -> [Sequance] -> [(Int, Int, Int, Int)] -> Score
        scoreSequances [] [] scores = convertToScore scores
        scoreSequances (sequance:xs) opponentSequances scores =
            scoreSequances xs opponentSequances (updateScores scores (maximum [5 - (sequanceLength sequance), 0]) (openSequance sequance) 1)
        scoreSequances [] (opponentSequance:xs) scores =
            scoreSequances [] xs (updateScores scores (maximum [5 - (sequanceLength opponentSequance), 0]) (openSequance opponentSequance) 2) 

convertToScore :: [(Int, Int, Int, Int)] -> Score
convertToScore [fives, fours, threes, twos, ones] = (fives, fours, threes, twos, ones)

nextMoves :: Board -> Int -> [(Int, Int)]
nextMoves board range = nextMoves board range ((length board)^2 - 1) (length board)
    where
        nextMoves :: [[Int]] -> Int -> Int -> Int -> [(Int, Int)]
        nextMoves _ _ (-1) _ = []
        nextMoves board range current total = if isNextMove board range (x, y) then
            (x, y):(nextMoves board range (current-1) total)
        else (nextMoves board range (current-1) total)
            where { x = current `mod` total; y = current  `div` total; }

isNextMove :: Board -> Int -> (Int, Int) -> Bool
isNextMove board range (x, y) = if (board!!y)!!x /= 0 then False else isNextMove board (neighbours (x, y) (length board) range)
    where
        isNextMove :: Board -> [(Int, Int)] -> Bool
        isNextMove _ [] = False
        isNextMove board ((x, y):tl) = if (board!!y)!!x /= 0 then True else isNextMove board tl
  
neighbours :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbours (x, y) border range = [(x + i, y + j) | i <- [-range..range], j <- [-range..range], x + i >= 0, y + j >= 0, x + i < border, y + j < border]
