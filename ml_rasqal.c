/* */

#include "ml_rasqal.h"


Make_Val_final_pointer(rasqal_world, Ignore, Ignore, 0)

ML_0 (rasqal_new_world, Val_rasqal_world)
ML_1 (rasqal_free_world, Rasqal_world_val, Unit)

ML_1 (rasqal_world_open, Rasqal_world_val, Unit)
