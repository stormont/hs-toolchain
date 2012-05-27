
module Main
   where

import Control.Monad (guard, mzero)
import Control.Monad.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Time
import System (getArgs)
import System.Console.GetOpt
import System.Exit (ExitCode (ExitSuccess))
import System.Info (os)
import System.Process (system)


data Flag = Usage
          | Compile String
          | RunUT String
          | RunFT String
          | RunVCS String
          deriving (Show, Eq)


data Options = Options
   { optFlags   :: [Flag]
   , optCompile :: String
   } deriving (Show)


main :: IO ()
main = do
   args <- getArgs
   let (actions, nonOpts, msgs) = getOpt RequireOrder options args
   opts <- foldl (>>=) (return defaultOptions) $ reverse actions
   mapM_ putStrLn msgs
   if (optFlags opts) == []
     then do
      runMaybeT $ showUsage
      return ()
     else do
      startTime <- getCurrentTime
      r <- runMaybeT $ dispatchOpts (optCompile opts) (optFlags opts)
      endTime <- getCurrentTime
      putStrLn $ "Toolchain took " ++ (show $ diffUTCTime endTime startTime)
      case r of
         Nothing -> putStrLn "Toolchain FAILED"
         _       -> putStrLn "Toolchain SUCCEEDED"


options :: [OptDescr (Options -> IO Options)]
options =
   [ Option ['?'] ["help"]            (NoArg addUsage)                     "Show usage/help"
   , Option ['o'] ["options"]         (ReqArg addCompileOptions "options") "Options to use when compiling"
   , Option ['c'] ["compile"]         (ReqArg addCompile "file")           "Project entry file to compile"
   , Option ['U'] ["rununittest"]     (ReqArg addRunUT "cmd")              "Unit test entry command to run"
   , Option ['F'] ["runfunctest"]     (ReqArg addRunFT "cmd")              "Functional test entry command to run"
   , Option []    ["vcs"]             (ReqArg addRunVCS "cmd")             "Run VCS command"
   ]


defaultOptions = Options { optFlags = [], optCompile = "-O2 -threaded" }
addUsage opt = return opt { optFlags = (Usage : optFlags opt) }
addCompileOptions o opt = return opt { optCompile = o }
addCompile f opt = return opt { optFlags = ((Compile f) : optFlags opt) }
addRunUT c opt = return opt { optFlags = ((RunUT c) : optFlags opt) }
addRunFT c opt = return opt { optFlags = ((RunFT c) : optFlags opt) }
addRunVCS c opt = return opt { optFlags = ((RunVCS c) : optFlags opt) }


showUsage :: MaybeT IO ()
showUsage = do
   let header = "Usage: toolchain [OPTIONS...]"
   liftIO $ putStrLn $ usageInfo header options
   return ()


dispatchOpts :: String -> [Flag] -> MaybeT IO ()
dispatchOpts opts []        = return ()
dispatchOpts opts (x:xs) = do
   case x of
      Usage       -> showUsage
      Compile f   -> compileModule opts f
      RunUT c     -> execProc c
      RunFT c     -> execProc c
      RunVCS c    -> execProc c
   dispatchOpts opts xs


compileModule :: String -> FilePath -> MaybeT IO ()
compileModule options path = do
   if path /= []
     then execProc $  "ghc " ++ options
                   ++ " --make " ++ path
                   ++ " -o " ++ (parseExe path)
     else return ()


execProc :: String -> MaybeT IO ()
execProc cmd = do
   e <- liftIO $ system cmd
   guard (e == ExitSuccess)
   return ()


parseExe path = (++ suffix) $ reverse $ drop 1 $ dropWhile (/= '.') $ reverse path
   where suffix = if (take 5 os) == "mingw"
           then ".exe"  -- EXE extention for Windows (which shows up as mingw)
           else ""      -- Linux doesn't need an extension
