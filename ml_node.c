/* */


#include "ml_node.h"


Make_Val_final_pointer(librdf_node, Ignore, Ignore, 0)

ML_1 (librdf_new_node, Librdf_world_val, Val_option_librdf_node)
ML_1 (librdf_free_node, Librdf_node_val, Unit)

ML_2 (librdf_new_node_from_blank_identifier,
       Librdf_world_val, UString_option_val, Val_option_librdf_node)
/*
ML_3 (librdf_new_node_from_counted_blank_identifier,
       Librdf_world_val, UString_option_val, Int_val, Val_option_librdf_node)
ML_3 (librdf_new_node_from_counted_uri_string,
       Librdf_world_val, UString_val, Int_val, Val_option_librdf_node)
*/
ML_4 (librdf_new_node_from_literal,
       Librdf_world_val, UString_val, String_option_val, Bool_val, Val_option_librdf_node)
ML_1 (librdf_new_node_from_node, Librdf_node_val, Val_option_librdf_node)
ML_4 (librdf_new_node_from_normalised_uri_string,
       Librdf_world_val, UString_val, Librdf_uri_val, Librdf_uri_val, Val_option_librdf_node)
ML_4 (librdf_new_node_from_typed_literal,
       Librdf_world_val, UString_val, String_option_val, Librdf_uri_option_val, Val_option_librdf_node)
ML_2 (librdf_new_node_from_uri,
       Librdf_world_val, Librdf_uri_val, Val_option_librdf_node)
ML_3 (librdf_new_node_from_uri_local_name,
       Librdf_world_val, Librdf_uri_val, UString_val, Val_option_librdf_node)
ML_2 (librdf_new_node_from_uri_string,
       Librdf_world_val, UString_val, Val_option_librdf_node)
/*
librdf_node *       librdf_node_decode                  (librdf_world *world,
                                                         size_t *size_p,
                                                         unsigned char *buffer,
                                                         size_t length);

size_t              librdf_node_encode                  (librdf_node *node,
                                                         unsigned char *buffer,
                                                         size_t length);
*/
ML_2 (librdf_node_equals, Librdf_node_val, Librdf_node_val, Val_bool)
ML_1 (librdf_node_get_blank_identifier, Librdf_node_val, Val_ustring)
/*
unsigned char *     librdf_node_get_counted_blank_identifier
                                                        (librdf_node *node,
                                                         size_t *len_p);
*/
ML_1 (librdf_node_get_li_ordinal, Librdf_node_val, Val_int)
ML_1 (librdf_node_get_literal_value, Librdf_node_val, Val_option_ustring)
/*
unsigned char *     librdf_node_get_literal_value_as_counted_string
                                                        (librdf_node *node,
                                                         size_t *len_p);
*/
ML_1 (librdf_node_get_literal_value_as_latin1, Librdf_node_val, Val_option_ustring)
ML_1 (librdf_node_get_literal_value_datatype_uri, Librdf_node_val, Val_option_librdf_uri)
ML_1 (librdf_node_get_literal_value_is_wf_xml, Librdf_node_val, Val_bool)
ML_1 (librdf_node_get_literal_value_language, Librdf_node_val, Val_option_string)
ML_1 (librdf_node_get_type, Librdf_node_val, Val_librdf_node_type)
ML_1 (librdf_node_get_uri, Librdf_node_val, Val_option_librdf_uri)
ML_1 ( librdf_node_is_blank, Librdf_node_val, Val_bool)
ML_1 ( librdf_node_is_literal, Librdf_node_val, Val_bool)
ML_1 ( librdf_node_is_resource, Librdf_node_val, Val_bool)
/*
librdf_iterator *   librdf_node_new_static_node_iterator
                                                        (librdf_world *world,
                                                         librdf_node **nodes,
                                                         int size);
void                librdf_node_print                   (librdf_node *node,
                                                         FILE *fh);
int                 librdf_node_write                   (librdf_node *node,
                                                         raptor_iostream *iostr);
*/


