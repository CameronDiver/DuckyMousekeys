module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           GHC.IO.Handle.FD
import           System.IO                      ( openFile
                                                , IOMode(ReadMode)
                                                )
import           Text.Regex

import           Events                        as E
import           FFI
import           XDoTool                       as X

pathFromEntry :: Maybe String -> Maybe String
pathFromEntry Nothing      = Nothing
pathFromEntry (Just entry) = do
  let maybeLine =
        find (\a -> head a == "H: Handlers") $ map (splitOn "=") $ lines entry
  case maybeLine of
    Nothing  -> Nothing
    (Just v) -> do
      let [_, handlers] = v
      let eventFile = matchRegex (mkRegex "(event[0-9]+)") handlers
      fmap (\ev -> "/dev/input/" ++ head ev) $ eventFile


getDeviceFilePath :: IO (Maybe String)
getDeviceFilePath = do
  fileData <- readFile "/proc/bus/input/devices"
  let entries = splitOn "\n\n" fileData
  let entry = find
        (\a -> isJust (findString "mouse" a) && isJust (findString "Ducky" a))
        entries
  return $ pathFromEntry entry
  where findString search str = findIndex (isPrefixOf search) (tails str)

main :: IO ()
main = do
  path <- getDeviceFilePath
  maybe (error "Can't find device path") mainLoop path

mainLoop :: String -> IO ()
mainLoop path = do
  handle <- openFile path ReadMode
  -- Grab the file, so no events can be read by another process
  fd     <- handleToFd handle
  grabDeviceFile fd
  forever $ do
    eventBytes <- B.hGet handle packetSize
    event      <- readEvent eventBytes
    let parsedEvent = parseEvent event
    print parsedEvent
    mapM_ handleEvent parsedEvent

handleEvent :: MouseEvent -> IO ()
handleEvent (   MouseMove   m ) = handleMouseMove m
handleEvent ev@(MouseScroll m ) = handleMouseScroll ev
handleEvent ev@(MouseClick _ _) = handleMouseClick ev


handleMouseMove :: MouseMove -> IO ()
handleMouseMove = moveMouse . diff
 where
  diff (MouseX v) = (v, 0)
  diff (MouseY v) = (0, v)

handleMouseScroll :: MouseEvent -> IO ()
handleMouseScroll (MouseScroll v) = mouseScroll $ scrollDirToDirection v
 where
  scrollDirToDirection :: Int -> ScrollDirection
  scrollDirToDirection value | value > 0 = Up
                             | otherwise = Down

handleMouseClick :: MouseEvent -> IO ()
handleMouseClick (MouseClick b state) = stateToFn state (mapMouseButton b)

mapMouseButton E.LeftButton  = X.LeftButton
mapMouseButton E.RightButton = X.RightButton

stateToFn Pressed  = mouseDown
stateToFn Released = mouseUp
