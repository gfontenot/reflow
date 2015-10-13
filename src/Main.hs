main = putStrLn "It works!"

wrap :: Int -> String -> [String]
wrap n s = foldl (splitWordsAt n) [] . words $ s

splitWordsAt :: Int -> [String] -> String -> [String]
splitWordsAt _ [] x = [x]
splitWordsAt n xs x
  | lineIsTooLong n (unwords [last xs, x]) = xs ++ [x]
  | otherwise = init xs ++ [(unwords [last xs, x])]

lineIsTooLong :: Int -> String -> Bool
lineIsTooLong n xs = length xs > n
