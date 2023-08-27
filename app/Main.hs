module Main where
import System.IO
import System.Process 
import System.Directory (doesFileExist)


data SubmoduleConfig
  = Header String
  | Url String
  | Path String
  deriving (Eq, Show)
  

filterOutChars :: String -> String -> String
filterOutChars charsToFilter = filter (`notElem` charsToFilter)

-- Remove characters from the string
sanitizeString :: [String] -> String -> String
sanitizeString strings value = foldl (flip filterOutChars) value strings

-- | Apply parser to content of file
parseFile :: [String]-> Maybe SubmoduleConfig
parseFile x = case head x of 
  -- "[submodule" -> Header $ filterOutChars "\"" $ last x
  "[submodule" -> Just (Header $ sanitizeString ["\"", "]"] $ last x)
  "url"        -> Just (Url $ sanitizeString ["\""] $ last x)
  "path"       -> Just (Path $ sanitizeString ["\""] $ last x)
  _ -> Nothing


bfs :: (String -> Maybe SubmoduleConfig -> IO String) -> String -> IO [Maybe SubmoduleConfig]
bfs f path = do
  let moduleFilePath = path ++ "/.gitmodules"
  configFileExists <- doesFileExist moduleFilePath
  if configFileExists 
    then do 
  
      modulefile <- readFile moduleFilePath
      let arr = [words i | i <- lines modulefile]
      let parsedModules = map parseFile arr
      print path

      clonedModules <- mapM (f path) parsedModules

      -- mapM makes this breadth first 
      nextResults <- mapM (bfs f) [path ++ "/" ++ s | s <- filter (/= "") clonedModules]
      return (parsedModules ++ concat nextResults)
    else do
      return [Nothing]

  
gitClone :: String -> Maybe SubmoduleConfig -> IO String
gitClone basePath (Just (Path moduleConfig)) = do
  let command = "git submodule update --init " ++ show moduleConfig
  (_, Just hout, _, _) <- createProcess (shell command) { std_out = CreatePipe, cwd = Just basePath }
  -- TODO: handle the case where file is not found.
  output <- hGetContents hout
  putStrLn output
  putStrLn ("Current path: " ++ moduleConfig)
  return moduleConfig
gitClone _ _ = do
  let message = "Not a path, ignoring . . ."
  putStrLn message
  return ""

 
-- pretty printer for config object collection
pp :: Show a => [Maybe a] -> IO ()
pp = mapM_ pp
  where 
    pp :: Show a=> Maybe a -> IO ()
    pp (Just x) = putStrLn $ "Just " ++ show x 
    pp Nothing = putStrLn "Nothing"


main :: IO ()
main = do
  putStrLn "\n\n"
  -- result <- dfs gitClone "resources/gitmodules"
  let command = "pwd"
  (_, Just hout, _, _) <- createProcess (shell command) { std_out = CreatePipe }
  curdir <- hGetContents hout
  let formattedPath = sanitizeString ["\n", "\r"] curdir
  print formattedPath 
  -- result <- dfs gitClone curdir
  result <- bfs gitClone "/tmp/submodule-script-test"
  pp result
  putStrLn ""
