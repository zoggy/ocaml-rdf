/* */
#include <librdf.h>

#include "wrappers.h"


#ifndef ML_RASQAL
#define ML_RASQAL

rasqal_world* rasqal_world_val(value val);
value val_rasqal_world_copy(value val);

#endif