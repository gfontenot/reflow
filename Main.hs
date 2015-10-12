import qualified Data.Text as T (strip, pack, unpack)

main = putStrLn "It works!"

wrap :: String -> [String]
wrap = map trim . combine . splitAt 80

combine :: (a, a) -> [a]
combine (a, b) = [a, b]

trim :: String -> String
trim = T.unpack . T.strip . T.pack
