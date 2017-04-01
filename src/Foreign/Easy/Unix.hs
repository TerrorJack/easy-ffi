{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Strict #-}

module Foreign.Easy.Unix where
#ifdef mingw32_HOST_OS
#else
import Control.Exception (throwIO)
import Data.Bits ((.|.))
import Data.Functor (void)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Easy.Types (Module(..), FFIException(..))
import Foreign.Ptr (Ptr, nullPtr)

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

loadModule :: FilePath -> IO Module
loadModule p =
    fmap Module $
    assertFFI (/= nullPtr) $
    withCString p $ \buf -> c_dlopen buf $ c_RTLD_NOW .|. c_RTLD_GLOBAL

freeModule :: Module -> IO ()
freeModule m = void $ assertFFI (== 0) $ c_dlclose (getModule m)

findSymbol :: Module -> String -> IO (Ptr ())
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
            throwIO $ FFIException $ "Last error message: " ++ msg
#endif
