import SolvingSoup

sampleSoup :: [String]
sampleSoup = ["0gato00", "00h0000", "00odnum", "0glg000", "0ta0000", "otlt000", "o0o0o00", "00h0000"]

sampleWords :: [String]
sampleWords = ["hola", "gato", "mundo", "haskell"]

spaceString :: String -> String
spaceString "" = ""
spaceString (char : string) =
  " " ++ char : spaceString string

printMatrix :: [String] -> IO ()
printMatrix [] = return ()
printMatrix (word : words) =
  do putStrLn (spaceString word); printMatrix words

printWordPosition :: String -> [[[Int]]] -> IO ()
printWordPosition _ [] = return ()
printWordPosition word (x : xs)
  | not (null x) = do putStr (show (head x) ++ ", "); printWordPosition word xs
  | otherwise = printWordPosition word xs

printWordToSearch :: [String] -> Int -> IO ()
printWordToSearch [] _ = return ()
printWordToSearch (word : words) index =
  do putStr (show index ++ ". " ++ word ++ "\n"); printWordToSearch words (index + 1)

printPositions :: [String] -> [[[[Int]]]] -> IO ()
printPositions [] _ = return ()
printPositions _ [] = return ()
printPositions (target : targets) (position : positions) =
  if not (null position)
    then do putStr ("\n" ++ target ++ " se encuentra en las posiciones: "); printWordPosition target position; printPositions targets positions
    else printPositions targets positions

solveAndPrint :: [String] -> [String] -> IO ()
solveAndPrint soup wordsToSearch = do
  putStrLn "Solución de la sopa de letras: \n"
  printMatrix soup

  putStrLn "\n Horizontales:"
  printPositions sampleWords (solveHorizontal wordsToSearch soup sampleSoup 0)

  putStrLn "\n Verticales:"
  printPositions sampleWords (solveVertical wordsToSearch soup sampleSoup)

  putStrLn "\n Diagonales:"
  printPositions sampleWords (solveDiagonal wordsToSearch soup sampleSoup)

  putStrLn "\n Diagonales Inversas:"
  printPositions sampleWords (solveInverseDiagonal wordsToSearch soup sampleSoup)

main :: IO ()
main = solveAndPrint sampleSoup sampleWords
