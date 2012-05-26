
module Main
   where

import Control.Monad (guard)
import Control.Monad.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Time
import System (getArgs)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (system)

{- TODO
run unit tests
run functional tests
-}


main = do
   args <- getArgs
   if length args /= 5
     then showUsage
     else do
      startTime <- getCurrentTime
      r <- runMaybeT $ runToolChain args
      endTime <- getCurrentTime
      putStrLn ""
      putStrLn $ "Toolchain took " ++ (show $ diffUTCTime endTime startTime)
      case r of
         Just _ -> putStrLn "Toolchain SUCCEEDED"
         _      -> putStrLn "Toolchain FAILED"


showUsage = do
   putStrLn "Usage: toolchain <ghc_opt> <main_hs> <unittest_hs> <functest_hs> <svc_cmd>"
   putStrLn ""
   putStrLn "   ghc_opt\tThe GHC compile options to apply"
   putStrLn "\tDefault is '-O2 -threaded' if this is an empty string"
   putStrLn "   main_hs\tThe main .hs project file"
   putStrLn "   unittest_hs\tThe entry point .hs file for running unit tests/QuickCheck"
   putStrLn "   functest_hs\tThe entry point .hs file for running functional tests"
   putStrLn "   svc_cmd\tThe command to execute for firing off source versioning control"
   putStrLn ""


runToolChain :: [String] -> MaybeT IO ()
runToolChain args = do
   compileCodeModules $ take 4 args
   runSourceControl $ head $ drop 4 args


compileCodeModules :: [String] -> MaybeT IO ()
compileCodeModules (options:main:unitTests:funcTests:[]) = do
   opts <- if options == []
      then return ("-O2 -threaded")
      else return options
   let f = compileModule opts
   f main
   f unitTests
   f funcTests


runSourceControl :: String -> MaybeT IO ()
runSourceControl cmd = do
   if cmd /= []
     then execProc cmd
     else return ()


compileModule :: String -> FilePath -> MaybeT IO ()
compileModule options path = do
   if path /= []
     then do
      let outExe = (++ "exe") $ take (length path - 2) path
          cmd = "ghc " ++ options ++ " --make " ++ path ++ " -o " ++ outExe
      execProc cmd
     else return ()


execProc :: String -> MaybeT IO ()
execProc cmd = do
   e <- liftIO $ system cmd
   guard (e == ExitSuccess)
   return ()
