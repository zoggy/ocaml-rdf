/* */
#include <librdf.h>

#include "wrappers.h"


#ifndef ML_RASQAL
#define ML_RASQAL

#define Rasqal_world_val(val) (rasqal_world*) Pointer_val(val)
value Val_rasqal_world(rasqal_world* val);

#define Val_option_rasqal_world(val) Val_option(val, Val_rasqal_world)
#define Rasqal_world_option_val(val) Option_val(val, Rasqal_world_val, NULL)


#endif