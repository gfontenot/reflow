import Options.Applicative

data Config = Config
  { width :: Int
  , path :: FilePath
  }

config :: Parser Config
config = Config
    <$> option auto
        ( long "width"
        <> short 'w'
        <> metavar "WIDTH"
        <> value 80
        <> help "Specify a line width to use instead of the default 80 columns"
        )

    <*> argument str
        ( metavar "PATH"
        <> help "Path to input file"
        )

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> config)
      ( fullDesc
      <> progDesc "Intelligently wrap lines to a given length"
      )

run :: Config -> IO ()
run (Config w p) = do
    f <- readFile p
    putStrLn $ unlines $ wrapLine w =<< lines f

wrapLine :: Int -> String -> [String]
wrapLine w s = foldl (splitWordsAt w) [] $ words s

splitWordsAt :: Int -> [String] -> String -> [String]
splitWordsAt _ [] x = [x]
splitWordsAt w xs x
  | length (appendToLast xs x) > w = xs ++ [x]
  | otherwise = init xs ++ [appendToLast xs x]

appendToLast :: [String] -> String -> String
appendToLast xs x = unwords [last xs, x]
