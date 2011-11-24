/* */
#include <librdf.h>
#include "wrappers.h"

#ifndef ML_INIT
#define ML_INIT

#define Librdf_world_val(val) (librdf_world*) Pointer_val(val)
librdf_world* librdf_world_val(value val);

#endif