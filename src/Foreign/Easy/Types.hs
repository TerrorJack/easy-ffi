module Foreign.Easy.Types where

import Control.Exception (Exception)
import Foreign.Ptr (Ptr)

newtype Module = Module
    { getModule :: Ptr ()
    }

newtype FFIException = FFIException
    { getFFIException :: String
    } deriving (Show)

instance Exception FFIException
