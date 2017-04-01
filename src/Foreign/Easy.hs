{-# OPTIONS_GHC -Wno-unused-imports #-}
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

import Control.Exception (Exception)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CChar, CWchar)
import Foreign.Ptr (Ptr)

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

type TCHAR = CWchar

type WINBOOL = Bool

foreign import ccall interruptible "LoadModuleExW" c_LoadModuleExW
               :: LPCWSTR -> HANDLE -> DWORD -> IO HMODULE

foreign import ccall interruptible "GetProcAddress"
               c_GetProcAddress :: HMODULE -> LPCSTR -> IO FARPROC

foreign import ccall interruptible "FreeModule" c_FreeModule ::
               HMODULE -> IO WINBOOL

foreign import ccall interruptible "GetLastError" c_GetLastError ::
               IO DWORD

foreign import ccall interruptible "FormatMessageW"
               c_FormatMessageW ::
               DWORD ->
                 LPCVOID -> DWORD -> DWORD -> LPWSTR -> DWORD -> Ptr () -> IO DWORD

loadModule = undefined

freeModule = undefined

findSymbol = undefined
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
