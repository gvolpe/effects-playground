{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ReaderIO where

import           Control.Monad.Reader           ( ask )
import           Data.Monoid                    ( (<>) )
import           RIO
import           System.IO                      ( getLine )

type TraceId = Utf8Builder

data Env = Env
  { appTraceId :: TraceId
  , appLogFunc :: LogFunc
  }

class HasTraceId env where
  appTraceIdL :: Lens' env TraceId

instance HasTraceId Env where
  appTraceIdL = lens appTraceId (\x y -> x { appTraceId = y })

instance HasLogFunc Env where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

tracedRead :: (HasLogFunc env, HasTraceId env) => RIO env String
tracedRead = do
  tid <- view appTraceIdL
  logInfo (">>> Read -> Trace-Id: " <> tid)
  liftIO getLine

tracedWrite :: (HasLogFunc env, HasTraceId env) => Utf8Builder -> RIO env ()
tracedWrite i = do
  tid <- view appTraceIdL
  logInfo (">>> Write -> Trace-Id: " <> tid)
  logInfo i

echoR :: (HasLogFunc env, HasTraceId env) => RIO env ()
echoR = do
  i <- tracedRead
  case i of
    "" -> pure ()
    _  -> tracedWrite (displayShow i) >> echoR

