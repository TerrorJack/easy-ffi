#include "libffi-utils.h"
#include <stdalign.h>

size_t ffi_type_size(void) { return sizeof(ffi_type); }
size_t ffi_type_align(void) { return alignof(ffi_type); }
void ffi_type_struct_init(ffi_type *t, ffi_type **ts) {
  t->size = 0;
  t->alignment = 0;
  t->type = FFI_TYPE_STRUCT;
  t->elements = ts;
}
