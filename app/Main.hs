module Main where

import System.IO
import System.Environment
import System.Console.Haskeline

import System.CPUTime
import Data.Time.Clock
import System.Info
import System.Environment

import Control.Monad.IO.Class -- liftIO !!!

prompt = "hs>> "

commandsHelp = "Available commands are:\n\
    \:quit - exits the program\n\
    \:time - show current time"

loop :: InputT IO ()
loop = do
   minput <- getInputLine prompt
   case minput of
       Nothing -> return ()
       Just ":quit"     -> return ()
       Just ":ptime"    -> do liftIO getCPUTime >>= outputStrLn . show >> loop -- <- ok, this took a LONG time to figure out. getCPUTime returns IO Int, which needs to be transformed to String and then lifted to InputT IO String...
       Just ":time"     -> do liftIO getCurrentTime >>= outputStrLn . show >> loop
       Just ":env"      -> do
                                env <- liftIO getEnvironment
                                let tmp1 (k,v) = show k ++ " = " ++ show v
                                mapM_ outputStrLn (map tmp1 env)
                                loop
                                
       Just ":sysinfo"  -> do 
                                outputStrLn os
                                outputStrLn arch
                                outputStrLn compilerName
                                outputStrLn $ show compilerVersion
                                loop
       Just ":help"     -> do outputStrLn commandsHelp >> loop
       Just input       -> do outputStrLn (input ++ ": - unknown command. Try :help") >> loop

                        



main :: IO ()
main = runInputT defaultSettings loop

