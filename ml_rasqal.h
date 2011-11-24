/* */
#include <librdf.h>

#include "wrappers.h"


#ifndef ML_RASQAL
#define ML_RASQAL

#define Rasqal_world_val(val) (rasqal_world*) Pointer_val(val)
value Val_rasqal_world(rasqal_world* val);
#endif