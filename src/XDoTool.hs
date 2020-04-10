module XDoTool where

import           System.Process

data ScrollDirection = Up | Down deriving (Show,Eq)
data MouseButton = LeftButton | RightButton deriving (Show,Eq)

moveMouse :: (Int, Int) -> IO ()
moveMouse (x, y) =
  callWithArgs $ "mousemove_relative -- " ++ show x ++ " " ++ show y

mouseScroll :: ScrollDirection -> IO ()
mouseScroll dir = callWithArgs $ "click " ++ (mouseButtonToId $ Left dir)

mouseDown :: MouseButton -> IO ()
mouseDown button =
  callWithArgs $ "mousedown " ++ mouseButtonToId (Right button)

mouseUp :: MouseButton -> IO ()
mouseUp button = callWithArgs $ "mouseup " ++ mouseButtonToId (Right button)

mouseButtonToId :: Either ScrollDirection MouseButton -> String
mouseButtonToId (Left dir) | dir == Up   = "4"
                           | dir == Down = "5"
mouseButtonToId (Right side) | side == LeftButton  = "1"
                             | side == RightButton = "3"

callWithArgs args = do
  spawnProcess "xdotool" (words args)
  return ()
