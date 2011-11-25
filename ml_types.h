/* */
#include <librdf.h>
#include "wrappers.h"

#ifndef ML_TYPES
#define ML_TYPES
#include "ml_enums.h"

#define Raptor_world_val(val) (raptor_world*) Pointer_val(val)
#define Val_option_raptor_world(val) Val_option(val, Val_raptor_world)
#define Raptor_world_option_val(val) Option_val(val, Raptor_world_val, NULL)
value Val_raptor_world(raptor_world* val);

#define Rasqal_world_val(val) (rasqal_world*) Pointer_val(val)
#define Val_option_rasqal_world(val) Val_option(val, Val_rasqal_world)
#define Rasqal_world_option_val(val) Option_val(val, Rasqal_world_val, NULL)
value Val_rasqal_world(rasqal_world* val);

#define Librdf_world_val(val) (librdf_world*) Pointer_val(val)

#define Librdf_model_val(val) (librdf_model*) Pointer_val(val)
#define Val_option_librdf_model(val) Val_option(val, Val_librdf_model)

#define Librdf_node_val(val) (librdf_node*) Pointer_val(val)
#define Val_option_librdf_node(val) Val_option(val, Val_librdf_node)

#define Val_librdf_node_type Val_node_type
#define Librdf_node_type_val Node_type_val

#define Librdf_statement_val(val) (librdf_statement*) Pointer_val(val)
#define Val_option_librdf_statement(val) Val_option(val, Val_librdf_statement)

#define Librdf_storage_val(val) (librdf_storage*) Pointer_val(val)
#define Val_option_librdf_storage(val) Val_option(val, Val_librdf_storage)

#define Librdf_uri_val(val) (librdf_uri*) Pointer_val(val)
#define Val_option_librdf_uri(val) Val_option(val, Val_librdf_uri)
#define Librdf_uri_option_val(val) Option_val(val, Librdf_uri_val, NULL)
value Val_librdf_uri(librdf_uri* val);

#endif