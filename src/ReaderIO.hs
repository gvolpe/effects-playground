module ReaderIO
  ( echoR
  , Env (..)
  )
where

import           Control.Monad.Reader           ( ask )
import           Data.Monoid                    ( (<>) )
import           RIO                     hiding ( traceId )

newtype Env = Env { traceId :: String } deriving (Show)

tracedRead :: RIO Env String
tracedRead = do
  env <- ask
  liftIO $ putStrLn $ ">>> Read -> Trace-Id: " <> traceId env
  liftIO getLine

tracedWrite :: String -> RIO Env ()
tracedWrite i = do
  env <- ask
  liftIO $ putStrLn $ ">>> Write -> Trace-Id: " <> traceId env
  liftIO $ putStrLn i

echoR :: RIO Env ()
echoR = do
  i <- tracedRead
  case i of
    "" -> pure ()
    _  -> tracedWrite i >> echoR

