/* */


#include "ml_query.h"


Make_Val_final_pointer(librdf_query, Ignore, Ignore, 0)

ML_5 (librdf_new_query, Librdf_world_val,
      String_val, Librdf_uri_option_val, UString_val, Librdf_uri_option_val, Val_option_librdf_query)
ML_1 (librdf_free_query, Librdf_query_val, Unit)
ML_1 (librdf_new_query_from_query, Librdf_query_val, Val_option_librdf_query)
