/* */

#include "ml_raptor.h"


Make_Val_final_pointer(raptor_world, Ignore, Ignore, 0)
Make_Val_final_pointer(raptor_iostream, Ignore, Ignore, 0)

ML_0 (raptor_new_world, Val_option_raptor_world)
ML_1 (raptor_free_world, Raptor_world_val, Unit)

ML_1 (raptor_world_open, Raptor_world_val, Val_int)

ML_1 (raptor_free_iostream, Raptor_iostream_val, Unit)
ML_2 (raptor_new_iostream_to_file_handle,
      Raptor_world_val, File_val, Val_option_raptor_iostream)

