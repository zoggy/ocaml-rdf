/* */
#include <librdf.h>
#include "wrappers.h"

#ifndef ML_TYPES
#define ML_TYPES
#include "ml_enums.h"

#define Voidstar_val(val) (void*)Pointer_val(val)
value Val_voidstar(void * val);

#define Raptor_world_val(val) (raptor_world*) Pointer_val(val)
#define Val_option_raptor_world(val) Val_option(val, Val_raptor_world)
#define Raptor_world_option_val(val) Option_val(val, Raptor_world_val, NULL)
value Val_raptor_world(raptor_world* val);

#define Rasqal_world_val(val) (rasqal_world*) Pointer_val(val)
#define Val_option_rasqal_world(val) Val_option(val, Val_rasqal_world)
#define Rasqal_world_option_val(val) Option_val(val, Rasqal_world_val, NULL)
value Val_rasqal_world(rasqal_world* val);

#define Librdf_world_val(val) (librdf_world*) Pointer_val(val)
#define Val_option_librdf_world(val) Val_option(val, Val_librdf_world)
value Val_librdf_world(librdf_world* val);

#define Librdf_model_val(val) (librdf_model*) Pointer_val(val)
#define Val_option_librdf_model(val) Val_option(val, Val_librdf_model)

#define Librdf_node_val(val) (librdf_node*) Pointer_val(val)
#define Val_option_librdf_node(val) Val_option(val, Val_librdf_node)
value Val_librdf_node(librdf_node* val);
#define Librdf_node_option_val(val) Option_val(val, Librdf_node_val, NULL)

#define Val_librdf_node_type Val_node_type
#define Librdf_node_type_val Node_type_val

#define Librdf_hash_val(val) (librdf_hash*) Pointer_val(val)
#define Val_option_librdf_hash(val) Val_option(val, Val_librdf_hash)
value Val_librdf_hash(librdf_hash* val);
#define Librdf_hash_option_val(val) Option_val(val, Librdf_hash_val, NULL)

#define Librdf_query_val(val) (librdf_query*) Pointer_val(val)
#define Val_option_librdf_query(val) Val_option(val, Val_librdf_query)
value Val_librdf_query(librdf_query* val);
#define Librdf_query_option_val(val) Option_val(val, Librdf_query_val, NULL)

#define Librdf_query_results_val(val) (librdf_query_results*) Pointer_val(val)
#define Val_option_librdf_query_results(val) Val_option(val, Val_librdf_query_results)
value Val_librdf_query_results(librdf_query_results* val);
#define Librdf_query_results_option_val(val) Option_val(val, Librdf_query_results_val, NULL)

#define Librdf_iterator_val(val) (librdf_iterator*) Pointer_val(val)
#define Val_option_librdf_iterator(val) Val_option(val, Val_librdf_iterator)
value Val_librdf_iterator(librdf_iterator* val);
#define Librdf_iterator_option_val(val) Option_val(val, Librdf_iterator_val, NULL)

#define Librdf_statement_val(val) (librdf_statement*) Pointer_val(val)
#define Val_option_librdf_statement(val) Val_option(val, Val_librdf_statement)

#define Librdf_storage_val(val) (librdf_storage*) Pointer_val(val)
#define Val_option_librdf_storage(val) Val_option(val, Val_librdf_storage)

#define Librdf_storage_factory_val(val) (librdf_storage_factory*) Pointer_val(val)
#define Val_option_librdf_storage_factory(val) Val_option(val, Val_librdf_storage_factory)

#define Librdf_uri_val(val) (librdf_uri*) Pointer_val(val)
#define Val_option_librdf_uri(val) Val_option(val, Val_librdf_uri)
#define Librdf_uri_option_val(val) Option_val(val, Librdf_uri_val, NULL)
value Val_librdf_uri(librdf_uri* val);

#define Librdf_stream_val(val) (librdf_stream*) Pointer_val(val)
#define Val_option_librdf_stream(val) Val_option(val, Val_librdf_stream)
value Val_librdf_stream(librdf_stream* val);


#endif