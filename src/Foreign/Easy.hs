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
import Foreign.C.String
       (CString, withCString, withCWString, peekCString, peekCWString)
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
throwFFI :: String -> IO a
throwFFI = throwIO . FFIException
#ifdef mingw32_HOST_OS
type DWORD = Word32

type FARPROC = Ptr ()

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

foreign import ccall interruptible "LoadLibraryW" c_LoadLibraryW ::
               LPCWSTR -> IO HMODULE

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

loadModule p =
    fmap Module $
    assertFFI (/= nullPtr) $ withCWString p $ \buf -> c_LoadLibraryW buf

freeModule m = void $ assertFFI (== True) $ c_FreeLibrary $ getModule m

findSymbol m s =
    assertFFI (/= nullPtr) $
    withCString s $ \buf -> c_GetProcAddress (getModule m) buf

assertFFI :: (a -> Bool) -> IO a -> IO a
assertFFI p m = do
    r <- m
    if p r
        then pure r
        else do
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
                        throwFFI $
                            "Call to FormatMessageW failed with " ++
                            show err' ++ ", last call failed with " ++ show err
                    else bracket
                             (peek ps)
                             (\msgbuf -> do
                                  msgbuf' <- c_LocalFree msgbuf
                                  unless (msgbuf' == nullPtr) $ do
                                      err' <- c_GetLastError
                                      throwFFI $
                                          "Call to LocalFree failed with " ++
                                          show err')
                             (\msgbuf -> do
                                  msg <- peekCWString msgbuf
                                  throwFFI $
                                      "Last error code: " ++
                                      show err ++ ", error message: " ++ msg)
#else
foreign import capi "dlfcn.h value RTLD_NOW" c_RTLD_NOW :: CInt

foreign import capi "dlfcn.h value RTLD_GLOBAL" c_RTLD_GLOBAL ::
               CInt

foreign import ccall interruptible "dlopen" c_dlopen ::
               CString -> CInt -> IO (Ptr ())

foreign import ccall interruptible "dlsym" c_dlsym ::
               Ptr () -> CString -> IO (Ptr ())

foreign import ccall interruptible "dlclose" c_dlclose ::
               Ptr () -> IO CInt

foreign import ccall interruptible "dlerror" c_dlerror ::
               IO CString

loadModule p =
    fmap Module $
    assertFFI (/= nullPtr) $
    withCString p $ \buf -> c_dlopen buf $ c_RTLD_NOW .|. c_RTLD_GLOBAL

freeModule m = void $ assertFFI (== 0) $ c_dlclose (getModule m)

findSymbol m s =
    assertFFI (/= nullPtr) $ withCString s $ \buf -> c_dlsym (getModule m) buf

assertFFI :: (a -> Bool) -> IO a -> IO a
assertFFI p m = do
    r <- m
    if p r
        then pure r
        else do
            buf <- c_dlerror
            msg <- peekCString buf
            throwFFI $ "Last error message: " ++ msg
#endif
