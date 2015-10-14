main = putStrLn "It works!"

wrap :: Int -> String -> [String]
wrap n s = foldl (splitWordsAt n) [] $ words s

splitWordsAt :: Int -> [String] -> String -> [String]
splitWordsAt _ [] x = [x]
splitWordsAt n xs x
  | lineIsTooLong n (appendToLast xs x) = xs ++ [x]
  | otherwise = init xs ++ [appendToLast xs x]

appendToLast :: [String] -> String -> String
appendToLast xs x = unwords [last xs, x]

lineIsTooLong :: Int -> String -> Bool
lineIsTooLong n xs = length xs > n
