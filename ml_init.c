/* */

#include <librdf.h>
#include "wrappers.h"
#include "ml_init.h"
#include "ml_rasqal.h"



Make_Val_final_pointer(librdf_world, Ignore, Ignore, 0)

ML_0 (librdf_new_world, Val_librdf_world)
ML_1 (librdf_free_world, Librdf_world_val, Unit)
ML_1 (librdf_world_open, Librdf_world_val, Unit)
ML_2 (librdf_world_set_rasqal, Librdf_world_val, Rasqal_world_val, Unit)
ML_1 (librdf_world_get_rasqal, Librdf_world_val, Val_rasqal_world)