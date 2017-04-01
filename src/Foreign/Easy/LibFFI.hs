{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Strict #-}

module Foreign.Easy.LibFFI where

import Foreign.C.Types (CInt(..), CUInt(..), CSize(..))
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable (Storable)

data Type
    = Void
    | SInt8
    | UInt8
    | SInt16
    | UInt16
    | SInt32
    | UInt32
    | SInt64
    | UInt64
    | Float
    | Double
    | LongDouble
    | Pointer
    | Struct [Type]

data Value = forall t. Storable t =>
                       Value
    { getValue :: t
    }

foreign import ccall interruptible "ffi_type_size" c_ffi_type_size
               :: CSize

foreign import ccall interruptible "ffi_type_align"
               c_ffi_type_align :: CSize

foreign import ccall interruptible "ffi_type_struct_init"
               c_ffi_type_struct_init :: Ptr Type -> Ptr (Ptr Type) -> IO ()

foreign import ccall interruptible "ffi_prep_cif" c_ffi_prep_cif ::
               Ptr () -> CInt -> CUInt -> Ptr Type -> Ptr (Ptr Type) -> IO CInt

foreign import ccall interruptible "ffi_prep_cif_var"
               c_ffi_prep_cif_var ::
               Ptr () ->
                 CInt -> CUInt -> CUInt -> Ptr Type -> Ptr (Ptr Type) -> IO CInt

foreign import ccall interruptible "ffi_call" c_ffi_call ::
               Ptr () -> FunPtr (IO ()) -> Ptr () -> Ptr (Ptr ()) -> IO ()

foreign import ccall interruptible "& ffi_type_void"
               c_ffi_type_void :: Ptr Type

foreign import ccall interruptible "& ffi_type_sint8"
               c_ffi_type_sint8 :: Ptr Type

foreign import ccall interruptible "& ffi_type_uint8"
               c_ffi_type_uint8 :: Ptr Type

foreign import ccall interruptible "& ffi_type_sint16"
               c_ffi_type_sint16 :: Ptr Type

foreign import ccall interruptible "& ffi_type_uint16"
               c_ffi_type_uint16 :: Ptr Type

foreign import ccall interruptible "& ffi_type_sint32"
               c_ffi_type_sint32 :: Ptr Type

foreign import ccall interruptible "& ffi_type_uint32"
               c_ffi_type_uint32 :: Ptr Type

foreign import ccall interruptible "& ffi_type_sint64"
               c_ffi_type_sint64 :: Ptr Type

foreign import ccall interruptible "& ffi_type_uint64"
               c_ffi_type_uint64 :: Ptr Type

foreign import ccall interruptible "& ffi_type_float"
               c_ffi_type_float :: Ptr Type

foreign import ccall interruptible "& ffi_type_double"
               c_ffi_type_double :: Ptr Type

foreign import ccall interruptible "& ffi_type_longdouble"
               c_ffi_type_longdouble :: Ptr Type

foreign import ccall interruptible "& ffi_type_pointer"
               c_ffi_type_pointer :: Ptr Type
