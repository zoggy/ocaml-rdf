/* */


#include "ml_stream.h"


Make_Val_final_pointer(librdf_stream, Ignore, Ignore, 0)

/*
ML_2 (librdf_new_hash, Librdf_world_val, String_val, Val_option_librdf_hash)
*/
/*ML_1 (librdf_new_hash_from_hash, Librdf_hash_val, Val_option_librdf_hash)*/
ML_1 (librdf_free_stream, Librdf_stream_val, Unit)
ML_1 (librdf_stream_end, Librdf_stream_val, Val_bool)
ML_1 (librdf_stream_next, Librdf_stream_val, Val_bool)
ML_1 (librdf_stream_get_object, Librdf_stream_val, Val_option_librdf_statement)
ML_1 (librdf_stream_get_context2, Librdf_stream_val, Val_option_librdf_node)