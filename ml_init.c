/* */

#include <librdf.h>
#include "wrappers.h"
#include "ml_init.h"
#include "ml_rasqal.h"

ML_1 (librdf_free_world, librdf_world_val, Unit)

Make_Val_refcount_pointer(librdf_world, Ignore, librdf_free_world, 0)

ML_0 (librdf_new_world, val_librdf_world)

ML_1 (librdf_world_open, librdf_world_val, Unit)
ML_2 (librdf_world_set_rasqal, librdf_world_val, rasqal_world_val, Unit)
ML_1 (librdf_world_get_rasqal, librdf_world_val, val_rasqal_world_copy)