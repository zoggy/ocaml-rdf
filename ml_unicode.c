/* */

#include <librdf.h>
#include "wrappers.h"


CAMLprim value ml_librdf_latin1_to_utf8 (value v) {
  return (Val_ustring(librdf_latin1_to_utf8 (UString_val(v), string_length(v), NULL)));
}

CAMLprim value ml_librdf_utf8_to_latin1 (value v) {
  return (Val_ustring(librdf_utf8_to_latin1 (UString_val(v), string_length(v), NULL)));
}