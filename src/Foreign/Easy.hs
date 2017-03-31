{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}

module Foreign.Easy
    (
    ) where
#ifdef mingw32_HOST_OS
import Data.Word (Word32)
import Foreign.C.Types (CInt(..), CChar, CWchar)
import Foreign.Ptr (Ptr)

type DWORD = Word32

type FARPROC = Ptr ()

type HANDLE = Ptr ()

type HMODULE = Ptr ()

type LPCSTR = Ptr CChar

type LPCVOID = Ptr ()

type LPCWSTR = Ptr CWchar

type LPTSTR = Ptr TCHAR

type TCHAR = CWchar

type WINBOOL = Bool

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
                 LPCVOID -> DWORD -> DWORD -> LPWSTR -> DWORD -> Ptr () -> IO DWORD
#else
#endif
