module TicTacToe (gameOver, parsePosition, tryMove) where

import Data.List (nub)
import Text.Read (readMaybe)

import Board
import Cell
import Player
import Helpers

type Position = (Int, Int)

gameOver :: Board -> Bool
gameOver board = helper(rows board) || helper(cols board) || helper(diags board)
  where
    helper :: [[Cell]] -> Bool
    helper [] = False
    helper (x:xs)
      | nub x == [Taken X] || nub x == [Taken O] = True
      | otherwise = helper xs

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition x = position
      where
        first = (takeWhile (/= ' ') x)
        second = (dropWhile (/= ' ') x)
        final = "(" ++ first ++ "," ++ second ++ ")"
        position = readMaybe final :: Maybe (Int, Int)

replacer :: Int -> Cell -> Board -> Board
replacer index new_square (Board n table) = (Board n (helper index 0 new_square table))
  where
    helper :: Int -> Int -> Cell -> [Cell] -> [Cell]
    helper _ _ _ [] = []
    helper index num new_square (x:xs)
      | index == num = new_square:xs
      | otherwise = x:(helper index (num+1) new_square xs)

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove ltr (a,b) table@(Board n cs)
  | a < 0 || b < 0 = Nothing
  |square == Empty = Just (replacer index new_square table)
  | otherwise = Nothing
  where
    index = (a*n+b)
    square = cs !! index
    new_square = Taken ltr