module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.List (findIndex, isPrefixOf, tails)
import Data.Maybe (listToMaybe, isJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')

main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just command -> do
      case select command of
        Just f -> f (tail args)
        _ -> do
              putStrLn "Unknown command"
              exitFailure
    Nothing -> do
      putStrLn "no command provided"
      exitFailure

type Command = [String] -> IO ()

select :: String -> Maybe Command
select "echo" = Just echo
select "wc" = Just wc
select "grep" = Just grep
select _ = Nothing

echo :: Command
echo xs = putStrLn (unwords xs)

wc :: Command
wc xs = do
  contents <- mapM readFile' xs
  let counts = fmap (length . lines) contents
  mapM_ print counts

grep :: Command
grep [] = do
  putStrLn "searchterm missing"
  exitFailure
grep [_] = do
  putStrLn "no input files"
  exitFailure
grep (term : fileNames) = do
  let grepHelper fileName = do
        contents <- liftIO (readFile' fileName)
        let matchedLines = filter (isMatch term) (lines contents)
        modify ((fileName, matchedLines) :)
  matched <- execStateT (mapM grepHelper fileNames) []
  let printMatch (fileName, matches) = do
        mapM_ putStrLn $ fmap ((fileName ++ ": ") ++) matches
  mapM_ printMatch matched

isMatch :: (Eq a) => [a] -> [a] -> Bool
isMatch search str = isJust $ findIndex (isPrefixOf search) (tails str)
