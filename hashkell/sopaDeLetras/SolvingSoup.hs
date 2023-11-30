module SolvingSoup (transposeMatrix, isWordInRow, searchHorizontal, findSubString, solveHorizontal, solveVertical, solveDiagonal, solveInverseDiagonal) where

{--------------------------------------------------------------------------------------------------------}

transposeMatrix :: [String] -> [String]
transposeMatrix [] = []
transposeMatrix matrix
  | any null matrix = []
  | otherwise = map head matrix : transposeMatrix (map tail matrix)

{--------------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------------}

{- The main idea of this section of code is to get the diagonals from a list of strings (a matrix) and order them in other list that cotains every one of each -}

getDiagonals :: [String] -> Int -> Int -> [String]
getDiagonals [] _ _ = []
getDiagonals soup rowIndex colIndex = getDiagonalsHelper1 soup rowIndex colIndex

getReversedDiagonals :: [String] -> Int -> Int -> [String]
getReversedDiagonals [] _ _ = []
getReversedDiagonals soup rowIndex colIndex = getDiagonalsHelper2 soup rowIndex colIndex

{- This function returns all the diagonals going from left to right

Here we use list comprenhensions to get the diagonals in two groups,
1. The diagonals starting from the last row to the first one
2. The diagonals starting from the second column to the last one-}
getDiagonalsHelper1 :: [String] -> Int -> Int -> [String]
getDiagonalsHelper1 [] _ _ = []
getDiagonalsHelper1 soup rowIndex colIndex
  | rowIndex < length soup =
      [ soup !! i !! j
        | (i, j) <- zip [rows - (rowIndex + 1) .. rows - 1] [0 .. cols - 1]
      ]
        : getDiagonalsHelper1 soup (rowIndex + 1) 0
  | colIndex + 1 < length (head soup) =
      [ soup !! i !! j
        | (i, j) <- zip [0 .. rows - 1] [colIndex + 1 .. cols - 1]
      ]
        : getDiagonalsHelper1 soup rowIndex (colIndex + 1)
  | otherwise = []
  where
    cols = if null soup then 0 else length (head soup)
    rows = length soup

{- This function returns all the diagonals going from rigth to left

Here we use list comprenhensions too, and there are also two cases
1. The diagonals starting from the last column and the last column going to the frist column
2. the diagonals going from the last row to the first one -}
getDiagonalsHelper2 :: [String] -> Int -> Int -> [String]
getDiagonalsHelper2 [] _ _ = []
getDiagonalsHelper2 soup rowIndex colIndex
  | colIndex + 1 < length (head soup) =
      [ soup !! i !! j
        | (i, j) <- zip [rows - (rowIndex + 1), rows - (rowIndex + 2) .. 0] [cols - (colIndex + 1) .. cols - 1]
      ]
        : getDiagonalsHelper2 soup rowIndex (colIndex + 1)
  | rowIndex < length soup =
      [ soup !! i !! j
        | (i, j) <- zip [rows - (rowIndex + 1), rows - (rowIndex + 2) .. 0] [0 .. cols - 1]
      ]
        : getDiagonalsHelper2 soup (rowIndex + 1) colIndex
  | otherwise = []
  where
    cols = if null soup then 0 else length (head soup)
    rows = length soup

{--------------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------------}

{- Used to search for a determinated substring into some string (row) and returns all the positions (indexes) where the substring appears

Additionally, the function adds the number of the ith row, in order to give a better position for matrixes,
The function return the positions (coordenates) per cases, as next:

0. This is the first case and is used for horizontals rows of a matrix
1. This case is used to return vertical postions, here we use the colIndex as rowIndex and rowINdex as colIndex cuz the matrix for this cases is transposed
2. This case is used for descendent diagonals in a matrix, here we need to make some maths to get the rigth postions, this due to the diagonal transformation method used:

First, we need to know the way a matrix is tranforme to get its diagonals
for the next matrix -->  a b c
                         d e f
                         g h i

We gwt the next -->        a
                          d b
                         g e c
                          h f
                           i

So, if we compare both, we woul see that transformed marix are largers than originals one, sowe need to make some operations depending if the rowIndex is bigger than the original matrix
3. This is the last case, and it is used as before, but oriented to ascendent diagonalss-}
findSubString :: [String] -> String -> String -> Int -> Int -> [[Int]]
findSubString _ _ [] _ _ = []
findSubString originalSoup target row rowIndex method = segmentRow originalSoup 0 row method
  where
    {- This functions segmentate  the row by ommiting its head in each recursive calal -}
    segmentRow :: [String] -> Int -> String -> Int -> [[Int]]
    segmentRow _ _ [] _ = []
    segmentRow originalSoup index row method
      | method == 0 =
          if isPrefix target row || isPrefix (reverse target) row
            then [rowIndex, index] : segmentRow originalSoup (index + 1) (drop (length target) row) method
            else segmentRow originalSoup (index + 1) (tail row) method
      | method == 1 =
          if isPrefix target row || isPrefix (reverse target) row
            then [index, rowIndex] : segmentRow originalSoup (index + 1) (drop (length target) row) method
            else segmentRow originalSoup (index + 1) (tail row) method
      | method == 2 =
          if isPrefix target row || isPrefix (reverse target) row
            then
              [ if rowIndex > length originalSoup - 1
                  then index
                  else length originalSoup - (rowIndex + 1) + index,
                if rowIndex > length originalSoup - 1
                  then rowIndex - length originalSoup + index + 1
                  else index
              ]
                : segmentRow originalSoup (index + 1) (drop (length target) row) method
            else segmentRow originalSoup (index + 1) (tail row) method
      | method == 3 =
          if isPrefix target row || isPrefix (reverse target) row
            then
              [ if rowIndex > length originalSoup - 1
                  then length originalSoup - 1 - (rowIndex - (length originalSoup - 1)) - index
                  else length originalSoup - (index + 1),
                if rowIndex > length originalSoup - 1
                  then index
                  else length originalSoup - (rowIndex + 1) + index
              ]
                : segmentRow originalSoup (index + 1) (drop (length target) row) method
            else segmentRow originalSoup (index + 1) (tail row) method

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys) = x == y && isPrefix xs ys

{- This function i used to fins a substring (word) int a row of a letter soup -}
isWordInRow :: [String] -> String -> String -> Int -> Int -> [[Int]]
isWordInRow _ "" _ _ _ = []
isWordInRow _ _ "" _ _ = []
isWordInRow originalSoup target word rowIndex method =
  findSubString originalSoup target word rowIndex method

{--------------------------------------------------------------------------------------------------------}

searchHorizontal :: String -> [String] -> [String] -> Int -> Int -> [[[Int]]]
searchHorizontal "" _ _ _ _ = []
searchHorizontal _ [] _ _ _ = []
searchHorizontal word (row : rows) originalSoup index method =
  findSubString originalSoup word row index method : searchHorizontal word rows originalSoup (index + 1) method

{--------------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------------}

solveHorizontal words soup originalSoup method = map (\word -> searchHorizontal word soup originalSoup 0 method) words

solveVertical words soup originalSoup = solveHorizontal words (transposeMatrix soup) originalSoup 1

solveDiagonal words soup originalSoup = solveHorizontal words (getDiagonals soup 0 0) originalSoup 2

solveInverseDiagonal words soup originalSoup = solveHorizontal words (getReversedDiagonals soup 0 0) originalSoup 3

{--------------------------------------------------------------------------------------------------------}

test1 :: [String]
test1 =
  [ "abc",
    "def",
    "ghi"
  ]

test2 :: [String]
test2 =
  [ "123",
    "456",
    "789"
  ]

test3 :: [String]
test3 =
  ["rswthgtmcuocibkctodm", "eiihklanvjaxoofeegel", "mtwcxvevywohlhwunbcp", "vgxvksjuoybxnasholaf", "vagatohfchcoyklxtkgd", "hlazmrbxesslsgsesngm", "msnxzqdfeoljnsfsjxja", "gezrglgotioejgczlrql", "yknrjgtnrqcglwhvoiko", "koguicilutxsnkqwctzh", "yvwgfsafwrsddiuzopty", "qwkndhlnklcfsnholaav", "hwzvmqyhzamfrjzwxamq", "oumwzbhrxavpcigeesyo", "lgtomhdtzpmunyvmqhqw", "akbmboeaqrusrlzpxxnr", "kfnozrjwqfmiyboalohj", "dmfhhgczzqoivswxlbtr", "upymhfizstqssqueoohs", "eiiqegzdjiwtnsoezzoz"]

main :: IO ()
main = do
  putStrLn "Test 1:"
  print $ getDiagonals test1 0 0
  putStrLn "Test 2:"
  print $ getDiagonals test2 0 0
  putStrLn "Test 3:"
  print $ getDiagonals test3 0 0
  print $ length test3
  print $ length (head test3)
