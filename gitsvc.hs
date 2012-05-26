
module Main
   where

import Control.Monad (guard)
import Control.Monad.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Time
import System (getArgs)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (system)


main = do
   args <- getArgs
   if (length args) /= 3
     then do
      putStrLn $ "Args passed: " ++ show args
      showUsage
     else do
      startTime <- getCurrentTime
      r <- runMaybeT $ runGitSVC args
      endTime <- getCurrentTime
      putStrLn ""
      putStrLn $ "GitSVC took " ++ (show $ diffUTCTime endTime startTime)
      case r of
         Just _ -> putStrLn "GitSVC SUCCEEDED"
         _      -> putStrLn "GitSVC FAILED"

showUsage = do
   putStrLn "Usage: gitsvc <add_opts> <filematch> <comment>"
   putStrLn ""
   putStrLn "   add_opts\tThe options to use with 'git add'"
   putStrLn "\tDefault is '-v -A' if this is an empty string"
   putStrLn "   filematch\tThe file pattern to match with 'git add'"
   putStrLn "\tYou may encounter errors if you prefix this pattern with '*'"
   putStrLn "   comment\tComment to apply to the commit"
   putStrLn ""
   

runGitSVC :: [String] -> MaybeT IO ()
runGitSVC (options:filematch:comment:[]) = do
   opts <- if options == []
      then return ("-v -A")
      else return options
   execProc $ "git add " ++ opts ++ " *" ++ filematch
   execProc $ "git commit -m \"" ++ comment ++ "\""
   execProc $ "git push -u origin master"


execProc :: String -> MaybeT IO ()
execProc cmd = do
   e <- liftIO $ system cmd
   guard (e == ExitSuccess)
   return ()
