/* */


#include "ml_storage.h"


Make_Val_final_pointer(librdf_storage, Ignore, Ignore, 0)

ML_4 (librdf_new_storage, Librdf_world_val, String_val, String_val, String_val, Val_option_librdf_storage)
ML_4 (librdf_new_storage_with_options, Librdf_world_val, String_val, String_val, Librdf_hash_val, Val_option_librdf_storage)
ML_1 (librdf_free_storage, Librdf_storage_val, Unit)
ML_1 (librdf_new_storage_from_storage, Librdf_storage_val, Val_option_librdf_storage)
ML_4 (librdf_new_storage_from_factory, Librdf_world_val,
      Librdf_storage_factory_val, String_val, Librdf_hash_val, Val_option_librdf_storage)
ML_2 (librdf_storage_open, Librdf_storage_val, Librdf_model_val, Val_bool)
ML_1 (librdf_storage_close, Librdf_storage_val, Val_bool)
ML_2 (librdf_storage_add_statement, Librdf_storage_val, Librdf_statement_val, Val_int)
ML_2 (librdf_storage_add_statements, Librdf_storage_val, Librdf_stream_val, Val_bool)
ML_2 (librdf_storage_remove_statement, Librdf_storage_val, Librdf_statement_val, Val_bool)
ML_2 (librdf_storage_contains_statement, Librdf_storage_val, Librdf_statement_val, Val_int)
/*

int                 librdf_storage_enumerate            (librdf_world *world,
                                                         const unsigned int counter,
                                                         const char **name,
                                                         const char **label);
int                 librdf_storage_register_factory     (librdf_world *world,
                                                         const char *name,
                                                         const char *label,
                                                         void (*factory) (librdf_storage_factory*));
*/
