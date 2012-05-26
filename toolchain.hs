
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
          | CompileUT String
          | CompileFT String
          | RunUT String
          | RunFT String
          | RunSVC
          deriving (Show, Eq)


data Options = Options
   { optFlags :: [Flag]
   } deriving (Show)


main :: IO ()
main = do
   args <- getArgs
   let (actions, nonOpts, msgs) = getOpt RequireOrder options args
   opts <- foldl (>>=) (return defaultOptions) actions
   mapM_ putStrLn msgs
   if (optFlags opts) == []
     then do
      runMaybeT $ showUsage
      return ()
     else do
      startTime <- getCurrentTime
      r <- runMaybeT $ dispatchOpts $ optFlags opts
      endTime <- getCurrentTime
      putStrLn $ "Toolchain took " ++ (show $ diffUTCTime endTime startTime)
      case r of
         Nothing -> putStrLn "Toolchain FAILED"
         _       -> putStrLn "Toolchain SUCCEEDED"


options :: [OptDescr (Options -> IO Options)]
options =
   [ Option ['?'] ["help"]            (NoArg addUsage)             "Show usage/help"
   , Option ['c'] ["compile"]         (ReqArg addCompile "file")   "Project entry file to compile"
   , Option ['u'] ["compileunittest"] (ReqArg addCompileUT "file") "Unit test entry file to compile"
   , Option ['f'] ["compilefunctest"] (ReqArg addCompileFT "file") "Functional test entry file to compile"
   , Option ['U'] ["rununittest"]     (ReqArg addRunUT "cmd")      "Unit test entry command to run"
   , Option ['F'] ["runfunctest"]     (ReqArg addRunFT "cmd")      "Functional test entry command to run"
   , Option []    ["svc"]             (NoArg addRunSVC)            "Run SVC command"
   ]


defaultOptions = Options { optFlags = [] }
addUsage opt = return opt { optFlags = (Usage : optFlags opt) }
addCompile f opt = return opt { optFlags = ((Compile f) : optFlags opt) }
addCompileUT f opt = return opt { optFlags = ((CompileUT f) : optFlags opt) }
addCompileFT f opt = return opt { optFlags = ((CompileFT f) : optFlags opt) }
addRunUT c opt = return opt { optFlags = ((RunUT c) : optFlags opt) }
addRunFT c opt = return opt { optFlags = ((RunFT c) : optFlags opt) }
addRunSVC opt = return opt { optFlags = (RunSVC : optFlags opt) }


dispatchOpts :: [Flag] -> MaybeT IO ()
dispatchOpts []     = return ()
dispatchOpts (x:xs) = do
   case x of
      Usage -> showUsage
      _     -> do
         liftIO $ putStrLn $ "TODO: " ++ (show x)
         mzero
   dispatchOpts xs


showUsage :: MaybeT IO ()
showUsage = do
   let header = "Usage: toolchain [OPTIONS...]"
   liftIO $ putStrLn $ usageInfo header options
   return ()


runToolChain :: [String] -> MaybeT IO ()
runToolChain args = do
   compileCodeModules $ take 4 args
   runTestModules $ take 2 $ drop 2 args
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


runTestModules :: [String] -> MaybeT IO ()
runTestModules (unitTests:funcTests:[]) = do
   if unitTests /= []
     then execProc $ parseExe unitTests
     else return ()
   if funcTests /= []
     then execProc $ parseExe funcTests
     else return ()


runSourceControl :: String -> MaybeT IO ()
runSourceControl cmd = do
   if cmd /= []
     then execProc cmd
     else return ()


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
