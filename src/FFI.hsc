{-# LANGUAGE ForeignFunctionInterface #-}
module FFI where

import GHC.IO.FD
import Data.Int

import Unsafe.Coerce

#include <linux/input.h>

foreign import ccall "ioctl" c_ioctl :: Int32 -> Int64 -> Int -> IO ()

grabDeviceFile :: FD -> IO ()
grabDeviceFile fd = c_ioctl (unsafeCoerce fd) (#const EVIOCGRAB) 1

-- Incomplete...
data EvCode = EV_REL | EV_SYN | EV_KEY | EV_ABS | EV_MSC | EV_SW |Unknown deriving Show

evCodeToString code = case code of
  (#const EV_REL) -> EV_REL
  (#const EV_SYN) -> EV_SYN
  (#const EV_KEY) -> EV_KEY
  (#const EV_ABS) -> EV_ABS
  (#const EV_MSC) -> EV_MSC
  (#const EV_SW) -> EV_SW
  otherwise -> Unknown

