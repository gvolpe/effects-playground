module Tagless where

class Monad m => Console m where
  readLine :: m String
  writeLine :: String -> m ()

instance Console IO where
  readLine  = getLine
  writeLine = putStrLn

echoTagless :: Console m => m ()
echoTagless = do
  i <- readLine
  case i of
    "" -> pure ()
    _  -> writeLine i >> echoTagless
