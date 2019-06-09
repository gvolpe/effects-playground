module Cloud
  ( cloudEcho
  )
where

import           Prelude                 hiding ( read )

import           Control.Applicative            ( (<|>) )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( void )
import           Data.Monoid                    ( (<>) )
import           Transient.Base

read :: TransIO String
read = input (const True) ""

write :: String -> TransIO ()
write = liftIO . print

writeP :: String -> TransIO ()
writeP i = async (print $ i <> "-t1") <|> async (print $ i <> "-t2")

-- Not the best of the examples for Transient since it's more for concurrent and parallel computations
-- but this is how we can write the Teletype program using TransIO.
echo :: TransIO ()
echo = do
  i <- read
  case i of
    "" -> pure ()
    _  -> write i >> echo

-- To make it more interesting we write twice in parallel using <|> (alternative operator)
echoP :: TransIO ()
echoP = do
  i <- read
  case i of
    "" -> pure ()
    _  -> writeP i >> echoP

cloudEcho :: IO ()
cloudEcho = void . keep $ echoP

