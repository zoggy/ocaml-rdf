/* */
#include <librdf.h>

#include "wrappers.h"
#include "ml_init.h"

#ifndef ML_STATEMENT
#define ML_STATEMENT

#define Librdf_statement_val(val) (librdf_statement*) Pointer_val(val)
value Val_librdf_statement(librdf_statement* val);

#define Val_option_librdf_statement(val) Val_option(val, Val_librdf_statement)


#endif