module Main where

import Control.Wire

-- use functions from Wire instead of Prelude
import Prelude hiding ((.), id)

control whenInhibited whenProduced wire = loop wire clockSession
  where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of
        Left ex -> whenInhibited ex
        Right x -> do
          whenProduced x
          loop w session

main :: IO ()
main = control return (putStrLn . show) $
  when (< 5) . timeFrom 0

