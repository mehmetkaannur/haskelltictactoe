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
parsePosition x = helper(readMaybe (space x) :: Maybe Int)
  where
    space :: String -> String
    space [] = []
    space (a:as)
      | a == ' ' || a == '-'= space as
      | otherwise = a : space as
    
    helper :: Maybe Int -> Maybe Position
    helper Nothing = Nothing
    helper (Just a)
      | length (show a) /= 2 = Nothing
      | otherwise = Just (first, second)
      where
        first = read (takeWhile (/= ' ') x) :: Int
        second = read (dropWhile (/= ' ') x) :: Int


tryMove :: Player -> Position -> Board -> Maybe Board
tryMove = undefined
