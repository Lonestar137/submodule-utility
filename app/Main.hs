module Main where
import Data.Maybe (isJust)
import System.Process 
import System.IO


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



dfs :: (Maybe SubmoduleConfig -> IO String) -> String -> IO [Maybe SubmoduleConfig]
dfs f path = do
  modulefile <- readFile path
  let arr = [words i | i <- lines modulefile]
  let parsedModules = map parseFile arr
  print path
  clonedModules <- mapM f parsedModules

  nextResults <- mapM (dfs f) [ s ++ "/.gitmodules" | s <- filter (/= "") clonedModules]
  return (parsedModules ++ concat nextResults)

  -- return parsedModules ++ dfs f module


  
-- TODO: this needs to CD into the path before running the command.
gitClone :: Maybe SubmoduleConfig -> IO String
gitClone (Just (Path moduleConfig)) = do
  let command = "git submodule update --init " ++ show moduleConfig
  (_, Just hout, _, _) <- createProcess (shell command) { std_out = CreatePipe }
  output <- hGetContents hout
  putStrLn output
  putStrLn ("Current path: " ++ moduleConfig)
  return moduleConfig
gitClone _ = do
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
  result <- dfs gitClone ".gitmodules"
  pp result
  putStrLn ""
