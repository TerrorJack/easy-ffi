#ifndef libffi_utils_h
#define libffi_utils_h

#include <ffi.h>

size_t ffi_type_size(void);
size_t ffi_type_align(void);
void ffi_type_struct_init(ffi_type*, ffi_type**);

#endif
