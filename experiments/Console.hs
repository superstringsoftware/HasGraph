{-
Simple REPL stolen from here: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
-}

import System.IO
import System.Environment

import System.Console.Haskeline

evalString :: String -> IO String
evalString expr = case expr of 
    ":help" -> return "Available commands are:\n:quit - exits the program"
    ('n':'e':'w':' ':xs) -> return $ "Creating object " ++ xs
    _ -> return "Unknown command. Try :help"


main :: IO ()
main = runInputT defaultSettings loop
   where 
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop

{-
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = case expr of 
    ":help" -> return "Available commands are:\n:quit - exits the program"
    ('n':'e':'w':' ':xs) -> return $ "Creating object " ++ xs
    _ -> return "Unknown command. Try :help"
-- evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== ":quit") (readPrompt "HasGraph>>> ") evalAndPrint


main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

-}