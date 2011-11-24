/* */


#include "ml_model.h"


Make_Val_final_pointer(librdf_model, Ignore, Ignore, 0)

ML_3 (librdf_new_model, Librdf_world_val, Librdf_storage_val, String_val, Val_librdf_model)
ML_1 (librdf_new_model_from_model, Librdf_model_val, Val_librdf_model)
ML_1 (librdf_free_model, Librdf_model_val, Unit)

