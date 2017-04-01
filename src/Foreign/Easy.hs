{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Strict #-}

module Foreign.Easy
    ( Module(..)
    , FFIException(..)
    , loadModule
    , freeModule
    , findSymbol
    ) where

import Control.Exception (Exception, throwIO, bracket)
import Control.Monad (unless)
import Data.Bits ((.|.))
import Data.Functor (void)
import Data.Word (Word32)
import Foreign.C.String (CString, withCString, peekCWString)
import Foreign.C.Types (CInt(..), CChar, CWchar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

newtype Module = Module
    { getModule :: Ptr ()
    }

newtype FFIException = FFIException
    { getFFIException :: String
    } deriving (Show)

instance Exception FFIException

loadModule :: FilePath -> IO Module
freeModule :: Module -> IO ()
findSymbol :: Module -> String -> IO (Ptr ())
#ifdef mingw32_HOST_OS
type DWORD = Word32

type FARPROC = Ptr ()

type HANDLE = Ptr ()

type HMODULE = Ptr ()

type LPCSTR = Ptr CChar

type LPCVOID = Ptr ()

type LPCWSTR = Ptr CWchar

type LPWSTR = Ptr CWchar

type WINBOOL = Bool

foreign import capi "windows.h value FORMAT_MESSAGE_FROM_SYSTEM"
               c_FORMAT_MESSAGE_FROM_SYSTEM :: DWORD

foreign import capi
               "windows.h value FORMAT_MESSAGE_ALLOCATE_BUFFER"
               c_FORMAT_MESSAGE_ALLOCATE_BUFFER :: DWORD

foreign import ccall interruptible "LoadLibraryExW"
               c_LoadLibraryExW :: LPCWSTR -> HANDLE -> DWORD -> IO HMODULE

foreign import ccall interruptible "GetProcAddress"
               c_GetProcAddress :: HMODULE -> LPCSTR -> IO FARPROC

foreign import ccall interruptible "FreeLibrary" c_FreeLibrary ::
               HMODULE -> IO WINBOOL

foreign import ccall interruptible "GetLastError" c_GetLastError ::
               IO DWORD

foreign import ccall interruptible "FormatMessageW"
               c_FormatMessageW ::
               DWORD ->
                 LPCVOID ->
                   DWORD -> DWORD -> Ptr LPWSTR -> DWORD -> Ptr () -> IO DWORD

foreign import ccall interruptible "LocalFree" c_LocalFree ::
               Ptr a -> IO (Ptr a)

loadModule = undefined

freeModule m = void $ cannotbe False $ c_FreeLibrary $ getModule m

findSymbol m s =
    cannotbe nullPtr $
    withCString s $ \buf -> c_GetProcAddress (getModule m) buf

cannotbe
    :: Eq a
    => a -> IO a -> IO a
cannotbe v m = do
    r <- m
    if r == v
        then do
            err <- c_GetLastError
            alloca $ \ps -> do
                r' <-
                    c_FormatMessageW
                        (c_FORMAT_MESSAGE_FROM_SYSTEM .|.
                         c_FORMAT_MESSAGE_ALLOCATE_BUFFER)
                        nullPtr
                        err
                        0
                        ps
                        0
                        nullPtr
                if r' == 0
                    then do
                        err' <- c_GetLastError
                        throwIO $
                            FFIException $
                            "Call to FormatMessageW failed with " ++
                            show err' ++ ", last call failed with " ++ show err
                    else bracket
                             (peek ps)
                             (\msgbuf -> do
                                  msgbuf' <- c_LocalFree msgbuf
                                  unless (msgbuf' == nullPtr) $ do
                                      err' <- c_GetLastError
                                      throwIO $
                                          FFIException $
                                          "Call to LocalFree failed with " ++
                                          show err')
                             (\msgbuf -> do
                                  msg <- peekCWString msgbuf
                                  throwIO $
                                      FFIException $
                                      "Last error code: " ++
                                      show err ++ ", error message: " ++ msg)
        else pure r
#else
foreign import ccall interruptible "dlopen" c_dlopen ::
               CString -> CInt -> IO (Ptr ())

foreign import ccall interruptible "dlsym" c_dlsym ::
               Ptr () -> CString -> IO (Ptr ())

foreign import ccall interruptible "dlclose" c_dlclose ::
               Ptr () -> IO CInt

foreign import ccall interruptible "dlerror" c_dlerror ::
               IO CString

foreign import ccall interruptible "strerror" c_strerror ::
               CInt -> IO CString

loadModule = undefined

freeModule = undefined

findSymbol = undefined
#endif
