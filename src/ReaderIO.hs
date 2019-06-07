module ReaderIO where

import           Control.Monad.Reader           ( ask )
import           Data.Monoid                    ( (<>) )
import           RIO                     hiding ( traceId )

type TraceId = String

data Env = Env
  { traceId :: TraceId
  , other :: String
  } deriving (Show)

class HasTraceId env where
  traceIdL :: Lens' env TraceId

instance HasTraceId Env where
  traceIdL = lens traceId (\x y -> x { traceId = y })

tracedRead :: HasTraceId env => RIO env String
tracedRead = do
  tid <- view traceIdL
  liftIO $ putStrLn $ ">>> Read -> Trace-Id: " <> tid
  liftIO getLine

tracedWrite :: HasTraceId env => String -> RIO env ()
tracedWrite i = do
  tid <- view traceIdL
  liftIO $ putStrLn $ ">>> Write -> Trace-Id: " <> tid
  liftIO $ putStrLn i

echoR :: HasTraceId env => RIO env ()
echoR = do
  i <- tracedRead
  case i of
    "" -> pure ()
    _  -> tracedWrite i >> echoR

