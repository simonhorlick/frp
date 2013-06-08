import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class
import Control.Wire

-- use functions from Wire instead of Prelude
import Prelude hiding ((.), id)

import GHC.Float -- double2Float

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

  control return print $ positionWire . timeFrom 0

getJoystickDirections :: IO (Float, Float)
getJoystickDirections = do
  GLFW.swapBuffers -- Process window messages
  r <- take 2 `fmap` GLFW.getJoystickPosition GLFW.Joystick0 2
  return $
    case r of
      [x, y] -> (x, y)
      _      -> (0, 0)

positionWire :: Wire () IO Time (Float,Float)
positionWire = accum (+) (0, 0) . joystickWire

-- Scales input so small movements have little affect and large movements have
-- significant affect.
deadzone :: Float -> Float
deadzone x = if (abs x < deadzoneDefault) then 0 else x*x*x

deadzoneDefault = 0.16 -- best deadzone for my controller

joystickWire :: Wire () IO Time (Float,Float)
joystickWire = mkFixM $
  \dt t -> do
    (x, y) <- getJoystickDirections
    return (Right (deadzone x * double2Float dt, deadzone y * double2Float dt))

