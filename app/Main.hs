module Main where

import System.IO
import System.Environment
import System.Console.Haskeline

import System.CPUTime

import Control.Monad.IO.Class

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
       Just ":time"     -> do liftIO getCPUTime >>= outputStrLn . show >> loop -- <- ok, this took a LONG time to figure out. getCPUTime returns IO Int, which needs to be transformed to String and then lifted to InputT IO String...
       Just ":help"     -> do outputStrLn commandsHelp >> loop
       Just input       -> do outputStrLn (input ++ ": - unknown command. Try :help") >> loop

                        



main :: IO ()
main = runInputT defaultSettings loop

