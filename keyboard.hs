import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class
import Control.Wire

-- use functions from Wire instead of Prelude
import Prelude hiding ((.), id)

import qualified Graphics.UI.GLFW as GLFW

control whenInhibited whenProduced wire = loop wire clockSession
  where
    loop w' session' = do
      -- run a discrete time step of the wire
      (mx, w, session) <- stepSession w' session' ()
      case mx of
        Left ex -> whenInhibited ex
        Right x -> whenProduced x
      loop w session

main :: IO ()
main = do
  True <- GLFW.initialize

  True <- GLFW.openWindow GLFW.defaultDisplayOptions
    { GLFW.displayOptions_numRedBits   = 8
    , GLFW.displayOptions_numGreenBits = 8
    , GLFW.displayOptions_numBlueBits  = 8
    , GLFW.displayOptions_numDepthBits = 1 }

  control return (putStrLn . show) $
    keyWire . timeFrom 0

isKeyPressed :: IO (Maybe Char)
isKeyPressed = do
  GLFW.swapBuffers -- Process window messages
  a <- GLFW.keyIsPressed (GLFW.CharKey 'A')
  return (if a then Just 'A' else Nothing)

-- Create a wire that produces the time when a is pressed and inhibits otherwise
keyWire :: Wire () IO Time Time
keyWire = mkFixM $ -- Construct a stateless effectful wire from the given function.
  \dt t -> do
    k <- isKeyPressed 
    return (if k /= Nothing then Right t else Left ())

