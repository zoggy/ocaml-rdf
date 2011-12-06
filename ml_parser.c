/* */


#include "ml_parser.h"


Make_Val_final_pointer(librdf_parser, Ignore, Ignore, 0)

ML_4 (librdf_new_parser, Librdf_world_val,
      String_option_val, String_option_val, Librdf_uri_option_val, Val_option_librdf_parser)
ML_1 (librdf_free_parser, Librdf_parser_val, Unit)
ML_4 (librdf_parser_parse_into_model, Librdf_parser_val,
       Librdf_uri_val, Librdf_uri_option_val, Librdf_model_val, Val_int)
ML_4 (librdf_parser_parse_string_into_model, Librdf_parser_val,
       UString_val, Librdf_uri_option_val, Librdf_model_val, Val_int)       