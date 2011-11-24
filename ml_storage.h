/* */
#include <librdf.h>

#include "wrappers.h"


#ifndef ML_STORAGE
#define ML_STORAGE

#define Librdf_storage_val(val) (librdf_storage*) Pointer_val(val)
value Val_librdf_storage(librdf_storage* val);

#define Val_option_librdf_storage(val) Val_option(val, Val_librdf_storage)


#endif