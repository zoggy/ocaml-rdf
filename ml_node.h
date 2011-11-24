/* */
#include <librdf.h>

#include "wrappers.h"
#include "ml_init.h"

#ifndef ML_NODE
#define ML_NODE

#define Librdf_node_val(val) (librdf_node*) Pointer_val(val)
value Val_librdf_node(librdf_node* val);

#define Val_option_librdf_node(val) Val_option(val, Val_librdf_node)


#endif