/*********************************************************************************/
/*                OCaml-RDF                                                      */
/*                                                                               */
/*    Copyright (C) 2011 Institut National de Recherche en Informatique          */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU Lesser General Public License version        */
/*    2.1 or later as published by the Free Software Foundation.                 */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU Lesser General Public           */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*                                                                               */
/*********************************************************************************/

/* */
#include <librdf.h>
#include "wrappers.h"

#ifndef ML_TYPES
#define ML_TYPES
#include "ml_enums.h"

#define Void_val(val) (void*)Pointer_val(val)
Make_Val_option_h(void)
#define Void_option_val(val) Option_val(val, Void_val, NULL)
value Val_void(void* val);

#define Raptor_world_val(val) (raptor_world*) Pointer_val(val)
Make_Val_option_h(raptor_world)
#define Raptor_world_option_val(val) Option_val(val, Raptor_world_val, NULL)
value Val_raptor_world(raptor_world* val);

#define Raptor_iostream_val(val) (raptor_iostream*) Pointer_val(val)
Make_Val_option_h(raptor_iostream)
#define Raptor_iostream_option_val(val) Option_val(val, Raptor_iostream_val, NULL)
value Val_raptor_iostream(raptor_iostream* val);


#define Rasqal_world_val(val) (rasqal_world*) Pointer_val(val)
Make_Val_option_h(rasqal_world)
#define Rasqal_world_option_val(val) Option_val(val, Rasqal_world_val, NULL)
value Val_rasqal_world(rasqal_world* val);

#define Librdf_parser_val(val) (librdf_parser*) Pointer_val(val)
Make_Val_option_h(librdf_parser)
#define Librdf_parser_option_val(val) Option_val(val, Librdf_parser_val, NULL)
value Val_librdf_parser(librdf_parser* val);

#define Librdf_world_val(val) (librdf_world*) Pointer_val(val)
Make_Val_option_h(librdf_world)
value Val_librdf_world(librdf_world* val);

#define Librdf_model_val(val) (librdf_model*) Pointer_val(val)
Make_Val_option_h(librdf_model)
value Val_librdf_model(librdf_model* val);

#define Librdf_node_val(val) (librdf_node*) Pointer_val(val)
Make_Val_option_h(librdf_node)
value Val_librdf_node(librdf_node* val);
#define Librdf_node_option_val(val) Option_val(val, Librdf_node_val, NULL)

#define Val_librdf_node_type Val_node_type
#define Librdf_node_type_val Node_type_val

#define Librdf_hash_val(val) (librdf_hash*) Pointer_val(val)
Make_Val_option_h(librdf_hash)
value Val_librdf_hash(librdf_hash* val);
#define Librdf_hash_option_val(val) Option_val(val, Librdf_hash_val, NULL)

#define Librdf_query_val(val) (librdf_query*) Pointer_val(val)
Make_Val_option_h(librdf_query)
value Val_librdf_query(librdf_query* val);
#define Librdf_query_option_val(val) Option_val(val, Librdf_query_val, NULL)

#define Librdf_query_results_val(val) (librdf_query_results*) Pointer_val(val)
Make_Val_option_h(librdf_query_results)
value Val_librdf_query_results(librdf_query_results* val);
#define Librdf_query_results_option_val(val) Option_val(val, Librdf_query_results_val, NULL)

#define Librdf_iterator_val(val) (librdf_iterator*) Pointer_val(val)
Make_Val_option_h(librdf_iterator)
value Val_librdf_iterator(librdf_iterator* val);
#define Librdf_iterator_option_val(val) Option_val(val, Librdf_iterator_val, NULL)

#define Librdf_statement_val(val) (librdf_statement*) Pointer_val(val)
Make_Val_option_h(librdf_statement)
value Val_librdf_statement(librdf_statement* val);

#define Librdf_storage_val(val) (librdf_storage*) Pointer_val(val)
Make_Val_option_h(librdf_storage)
value Val_librdf_storage(librdf_storage* val);

#define Librdf_storage_factory_val(val) (librdf_storage_factory*) Pointer_val(val)
Make_Val_option_h(librdf_storage_factory)
value Val_librdf_storage_factory(librdf_storage_factory* val);

#define Librdf_uri_val(val) (librdf_uri*) Pointer_val(val)
Make_Val_option_h(librdf_uri)
#define Librdf_uri_option_val(val) Option_val(val, Librdf_uri_val, NULL)
value Val_librdf_uri(librdf_uri* val);

#define Librdf_stream_val(val) (librdf_stream*) Pointer_val(val)
Make_Val_option_h(librdf_stream)
value Val_librdf_stream(librdf_stream* val);


#endif