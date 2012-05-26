
module Main
   where

import Control.Monad (guard)
import Control.Monad.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Time
import System.Exit (ExitCode (ExitSuccess))
import System.Process (system)


main = do
   startTime <- getCurrentTime
   r <- runMaybeT $ runGitSVC
   endTime <- getCurrentTime
   putStrLn ""
   putStrLn $ "GitSVC took " ++ (show $ diffUTCTime endTime startTime)
   case r of
      Just _ -> putStrLn "GitSVC SUCCEEDED"
      _      -> putStrLn "GitSVC FAILED"
   

runGitSVC :: MaybeT IO ()
runGitSVC = do
   options <- liftIO $ getOptions
   filematch <- liftIO $ getFilematch
   opts <- if options == []
      then return ("-v -A")
      else return options
   execProc $ "git add " ++ options ++ " *" ++ filematch
   comment <- liftIO $ getComment
   execProc $ "git commit -m \"" ++ comment ++ "\""
   --execProc $ "git push -u origin master"


getOptions :: IO (String)
getOptions = do
   putStr "Enter git add options to use [defaults to -v -A]: "
   input <- getLine
   if input == []
     then return ("-v -A")
     else return (input)


getFilematch :: IO (String)
getFilematch = do
   putStr "Enter git add filematch to use: "
   getLine


getComment :: IO (String)
getComment = do
   putStr "Enter git commit comment to use: "
   getLine


execProc :: String -> MaybeT IO ()
execProc cmd = do
   e <- liftIO $ system cmd
   guard (e == ExitSuccess)
   return ()
