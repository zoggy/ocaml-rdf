/* */
#include <librdf.h>

#include "wrappers.h"


#ifndef ML_RAPTOR
#define ML_RAPTOR

#define Raptor_world_val(val) (raptor_world*) Pointer_val(val)
value Val_raptor_world(raptor_world* val);

#define Val_option_raptor_world(val) Val_option(val, Val_raptor_world)



#endif
