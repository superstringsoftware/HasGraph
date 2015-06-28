module Main where

import System.IO
import System.Environment
import System.Console.Haskeline

evalString :: String -> IO String
evalString expr = case expr of 
    ":help" -> return "Available commands are:\n:quit - exits the program"
    ('n':'e':'w':' ':xs) -> return $ "Creating object " ++ xs
    _ -> return "Unknown command. Try :help"

loop :: InputT IO ()
loop = do
   minput <- getInputLine "hs>> "
   case minput of
       Nothing -> return ()
       Just ":quit" -> return ()
       Just ":help" -> do outputStrLn "Available commands are:\n:quit - exits the program" >> loop
       Just input -> do outputStrLn $ input ++ ": - unknown command. Try :help"
                        loop



main :: IO ()
main = runInputT defaultSettings loop

