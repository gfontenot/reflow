import qualified Data.Text as T (strip, pack, unpack)

main = putStrLn "It works!"

wrap :: Int -> String -> [String]
wrap n s = removeEmpty . map trim . foldl (splitWordsAt n) [] . words $ s

splitWordsAt :: Int -> [String] -> String -> [String]
splitWordsAt _ [] x = [x]
splitWordsAt n xs x
  | lineIsTooLong n (unwords [last xs, x]) = xs ++ [x]
  | otherwise = init xs ++ [(unwords [last xs, x])]

lineIsTooLong :: Int -> String -> Bool
lineIsTooLong n xs = case length xs `compare` n of
  GT -> True
  otherwise -> False

trim :: String -> String
trim = T.unpack . T.strip . T.pack

removeEmpty :: [[a]] -> [[a]]
removeEmpty = filter (not . null)
