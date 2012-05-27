
module Main
   where

import Control.Monad (guard)
import Control.Monad.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Text (pack, toLower)
import Data.Time
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hFlush, stdout)
import System.Process (system)


main :: IO ()
main = do
   startTime <- getCurrentTime
   r <- runMaybeT $ runGitVCS
   endTime <- getCurrentTime
   putStrLn ""
   putStrLn $ "GitVCS took " ++ (show $ diffUTCTime endTime startTime)
   case r of
      Just _ -> putStrLn "GitVCS SUCCEEDED"
      _      -> putStrLn "GitVCS FAILED"
   

runGitVCS :: MaybeT IO ()
runGitVCS = do
   liftIO $ putStr "Run diff first? Default is no. [y|n]: "
   liftIO $ hFlush stdout
   diff <- liftIO $ getLine
   if any (== (toLower $ pack diff)) (map pack ["y", "yes"])
     then execProc "git diff"
     else return ()
   liftIO $ putStr "Enter git add options to use [defaults to -v -A]: "
   liftIO $ hFlush stdout
   options <- liftIO $ getOptions
   liftIO $ putStr "Enter git add filematch to use: "
   liftIO $ hFlush stdout
   filematch <- liftIO $ getLine
   execProc $ "git add " ++ options ++ " *" ++ filematch
   liftIO $ putStr "Enter git commit comment to use: "
   liftIO $ hFlush stdout
   comment <- liftIO $ getLine
   execProc $ "git commit -m \"" ++ comment ++ "\""
   execProc $ "git push -u origin master"


getOptions :: IO (String)
getOptions = do
   input <- getLine
   if input == []
     then return ("-v -A")
     else return (input)


execProc :: String -> MaybeT IO ()
execProc cmd = do
   e <- liftIO $ system cmd
   guard (e == ExitSuccess)
   return ()
