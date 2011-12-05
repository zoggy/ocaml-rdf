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
ML_2 (librdf_storage_add_statements, Librdf_storage_val, Librdf_stream_val, Val_int)
ML_2 (librdf_storage_remove_statement, Librdf_storage_val, Librdf_statement_val, Val_int)
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
ML_1 (librdf_storage_serialise, Librdf_storage_val, Val_option_librdf_stream)
ML_2 (librdf_storage_find_statements, Librdf_storage_val, Librdf_statement_val, Val_option_librdf_stream)
ML_4 (librdf_storage_find_statements_with_options, Librdf_storage_val,
      Librdf_statement_val, Librdf_node_option_val, Librdf_hash_option_val, Val_option_librdf_stream)
ML_3 (librdf_storage_get_sources, Librdf_storage_val, Librdf_node_val, Librdf_node_val,
      Val_option_librdf_iterator)
ML_3 (librdf_storage_get_arcs, Librdf_storage_val, Librdf_node_val, Librdf_node_val,
      Val_option_librdf_iterator)
ML_3 (librdf_storage_get_targets, Librdf_storage_val, Librdf_node_val, Librdf_node_val,
      Val_option_librdf_iterator)

ML_2 (librdf_storage_get_arcs_in, Librdf_storage_val, Librdf_node_val, Val_option_librdf_iterator)
ML_2 (librdf_storage_get_arcs_out, Librdf_storage_val, Librdf_node_val, Val_option_librdf_iterator)

ML_3 (librdf_storage_has_arc_in, Librdf_storage_val, Librdf_node_val, Librdf_node_val, Val_bool)
ML_3 (librdf_storage_has_arc_out, Librdf_storage_val, Librdf_node_val, Librdf_node_val, Val_bool)

ML_3 (librdf_storage_context_add_statement, Librdf_storage_val, Librdf_node_val, Librdf_statement_val, Val_int)
ML_3 (librdf_storage_context_add_statements, Librdf_storage_val, Librdf_node_val, Librdf_stream_val, Val_int)
ML_3 (librdf_storage_context_remove_statement, Librdf_storage_val, Librdf_node_val, Librdf_statement_val, Val_int)
ML_2 (librdf_storage_context_as_stream, Librdf_storage_val, Librdf_node_val, Val_option_librdf_stream)

ML_2 (librdf_storage_supports_query, Librdf_storage_val, Librdf_query_val, Val_bool)
ML_2 (librdf_storage_query_execute, Librdf_storage_val, Librdf_query_val, Val_option_librdf_query_results)

ML_1 (librdf_storage_sync, Librdf_storage_val, Val_int)

ML_3 (librdf_storage_find_statements_in_context, Librdf_storage_val,
      Librdf_statement_val, Librdf_node_option_val, Val_option_librdf_stream)

ML_1 (librdf_storage_get_contexts, Librdf_storage_val, Val_option_librdf_iterator)

ML_2 (librdf_storage_get_feature, Librdf_storage_val, Librdf_uri_val, Val_option_librdf_node)
ML_3 (librdf_storage_set_feature, Librdf_storage_val, Librdf_uri_val, Librdf_node_val, Val_int)

ML_1 (librdf_storage_transaction_commit, Librdf_storage_val, Val_int)
ML_1 (librdf_storage_transaction_get_handle, Librdf_storage_val,  Val_voidstar)
ML_1 (librdf_storage_transaction_rollback, Librdf_storage_val, Val_int)
ML_1 (librdf_storage_transaction_start, Librdf_storage_val, Val_int)
ML_2 (librdf_storage_transaction_start_with_handle, Librdf_storage_val, Voidstar_val, Val_int)

ML_1 (librdf_storage_get_world, Librdf_storage_val, Val_librdf_world)

