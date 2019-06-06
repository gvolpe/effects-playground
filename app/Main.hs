module Main where

import           Prelude                 hiding ( read )

import           Control.Effect                 ( run )
import           Fusion                         ( runTeletypeRet
                                                , read
                                                , write
                                                )
import           Poly                           ( echoIO )
import           Polysemy                       ( runM )

fusedMain :: IO ()
fusedMain = do
  putStrLn "Hello, Haskell!"
  print $ run (runTeletypeRet ["input"] read)                        -- single input
  print $ run (runTeletypeRet ["input"] (write "output"))            -- single output
  print $ run (runTeletypeRet ["in"] (write "out1" >> write "out2")) -- multiple outputs

-- echo forever
polyMain :: IO ()
polyMain = runM echoIO

main :: IO ()
main = polyMain
