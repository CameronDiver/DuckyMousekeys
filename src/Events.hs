module Events where

import           Control.Monad
import           Data.ByteString.Lazy          as B
import           Data.Binary.Get
import           Data.Word
import           Data.Int
import           Debug.Trace                    ( traceShow )

import           FFI

data Event = Event {
  evType :: EvCode,
  evCode :: Int16,
  evValue :: Int32
} deriving Show

type IntEvent = (Word64, Word64, Word16, Word16, Word32)

data MouseEvent = MouseMove MouseMove | MouseScroll Int | MouseClick MouseButton ButtonState
  deriving Show
data MouseMove = MouseX Int | MouseY Int
  deriving Show
data MouseButton = LeftButton | RightButton
  deriving Show
data ButtonState = Pressed | Released
  deriving Show

packetSize :: Int
-- This should really be generated...
-- 2 * (short) + int + timeval
packetSize = 2 * 2 + 4 + 16


readEvent :: ByteString -> IO Event
readEvent event = return $ intEventToEvent (runGet readParts event)


readParts :: Get IntEvent
readParts = do
  seconds <- getWord64le
  micros  <- getWord64le
  evType  <- getWord16le
  evCode  <- getWord16le
  value   <- getWord32le
  return (seconds, micros, evType, evCode, value)


intEventToEvent :: IntEvent -> Event
intEventToEvent (_, _, t, c, v) =
  Event (evCodeToString $ fromIntegral t) (fromIntegral c) (fromIntegral v)

parseEvent :: Event -> Maybe MouseEvent
parseEvent (Event EV_REL 0 v) = Just $ MouseMove $ MouseX $ fromIntegral v
parseEvent (Event EV_REL 1 v) = Just $ MouseMove $ MouseY $ fromIntegral v
parseEvent (Event EV_REL 8 v) = Just $ MouseScroll $ fromIntegral v
parseEvent (Event EV_KEY 272 v) =
  Just $ MouseClick LeftButton (valueToButtonState $ fromIntegral v)
parseEvent (Event EV_KEY 273 v) =
  Just $ MouseClick RightButton (valueToButtonState $ fromIntegral v)
parseEvent ev = Nothing

valueToButtonState :: Int -> ButtonState
valueToButtonState i | i == 1    = Pressed
                     | i == 0    = Released
                     | otherwise = error $ "Invalid button state: " ++ show i
