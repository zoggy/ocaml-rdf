/* */


#include "ml_iterator.h"


Make_Val_final_pointer(librdf_iterator, Ignore, Ignore, 0)

ML_1 (librdf_free_iterator, Librdf_iterator_val, Unit)
ML_1 (librdf_iterator_end, Librdf_iterator_val, Val_bool)
ML_1 (librdf_iterator_next, Librdf_iterator_val, Val_bool)
ML_1 (librdf_iterator_get_object, Librdf_iterator_val, Val_option_voidstar)
ML_1 (librdf_iterator_get_context, Librdf_iterator_val, Val_option_voidstar)
ML_1 (librdf_iterator_get_key, Librdf_iterator_val, Val_option_voidstar)
ML_1 (librdf_iterator_get_value, Librdf_iterator_val, Val_option_voidstar)
