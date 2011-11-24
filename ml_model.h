/* */
#include <librdf.h>

#include "wrappers.h"
#include "ml_init.h"
#include "ml_storage.h"

#ifndef ML_MODEL
#define ML_MODEL

#define Librdf_model_val(val) (librdf_model*) Pointer_val(val)
value Val_librdf_model(librdf_model* val);

#define Val_option_librdf_model(val) Val_option(val, Val_librdf_model)


#endif