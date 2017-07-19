module Lib
  ( solutions
  , printBoard
  , printBoard'
  , example
  ) where

import Data.List

example :: [[Int]]
example =
  [ [4, 0, 0, 0, 0, 0, 8, 0, 5]
  , [0, 3, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 7, 0, 0, 0, 0, 0]
  , [0, 2, 0, 0, 0, 0, 0, 6, 0]
  , [0, 0, 0, 0, 8, 0, 4, 0, 0]
  , [0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 6, 0, 3, 0, 7, 0]
  , [5, 0, 0, 2, 0, 0, 0, 0, 0]
  , [1, 0, 4, 0, 0, 0, 0, 0, 0]
  ]

printBoard :: [[Int]] -> IO ()
printBoard board = do
  mapM print board
  return ()

printBoard' :: [[Int]] -> IO [()]
printBoard' board = mapM print board

columns :: [[Int]] -> [[Int]]
columns board
  | null $ head board = []
  | otherwise = (map head board) : (columns $ map tail board)

boxes :: [[Int]] -> [[Int]]
boxes board =
  let takeBox x y n =
        foldr (++) [] $
        map (\row -> (take n $ drop x row)) $
        columns $ map (\row -> (take n $ drop y row)) board
  in foldr (++) [] $
     map (\x -> (map (\y -> (takeBox x y 3)) xs)) xs
     where xs = [0, 3, 6]

boxes' :: [[Int]] -> [[Int]]
boxes' board =
  foldr (++) [] $ map f xs
  where takeBox x y n =
          foldr (++) [] $
          map (\row -> (take n $ drop x row)) $
          columns $ map (\row -> (take n $ drop y row)) board
        f x = map (\y -> (takeBox x y 3)) xs
        xs = [0, 3, 6]

groups :: [[Int]] -> [[Int]]
groups board = board ++ (columns board) ++ (boxes board)

type Index = (Int, Int)

peers :: Index -> [Index]
peers index = delete index $ nub indexGroups
  where indexGroups = undefined

type Board = [[Int]]

boardRef :: Board -> Index -> Int
boardRef board index = (board !! x) !! y
  where (x, y) = index

boardReplace :: Board -> Index -> Int -> Board
boardReplace board index point =
  listReplace board x (listReplace row y point)
  where
    listReplace list n v = (take n list) ++ [v] ++ (drop (n + 1) list)
    (x, y) = index
    row = board !! x

prevIndex :: Index -> Maybe Index
prevIndex (0, 0) = Nothing
prevIndex (x, 0) = Just (x - 1, 8)
prevIndex (x, y) = Just (x, y - 1)

prevIndex' :: Index -> Maybe Index
prevIndex' index = case index of
                     (0, 0) -> Nothing
                     (x, 0) -> Just (x - 1, 8)
                     (x, y) -> Just (x, y - 1)

solutions :: Board -> [Board]
solutions board = helper $ Just (8, 8)
  where helper maybeIndex =
          case maybeIndex of
            Nothing -> [board]
            Just index -> undefined
