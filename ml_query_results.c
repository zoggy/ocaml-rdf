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

#include "ml_query_results.h"

#define QR_val Librdf_query_results_val

Make_Val_final_pointer(librdf_query_results, Ignore, Ignore, 0)

ML_1 (librdf_free_query_results, QR_val, Unit)

ML_1 (librdf_query_results_as_stream, QR_val, Val_option_librdf_stream)
ML_1 (librdf_query_results_get_count, QR_val, Val_int)
ML_1 (librdf_query_results_next, QR_val, Val_bool)
ML_1 (librdf_query_results_finished, QR_val, Val_bool)
ML_1 (librdf_query_results_get_bindings_count, QR_val, Val_int)

CAMLprim value val_option_node (char const * n) {
  return (Val_option_librdf_node((librdf_node* )n));
}

value ml_librdf_query_results_get_bindings (value v) {
  CAMLparam1(v);
  CAMLlocal1(tuple) ;
  librdf_query_results* qr = Librdf_query_results_val(v);
  int cpt = librdf_query_results_get_bindings_count(qr);
  const char **names=NULL; librdf_node* values[cpt];
  int ret = librdf_query_results_get_bindings (qr, &names, values);
  if (ret) {
    CAMLreturn (Val_unit);
  } else
  {
    tuple = alloc_tuple(2); /* The pair (names, nodes) */
    Store_field(tuple, 0, caml_alloc_array(&caml_copy_string, names));
    Store_field(tuple, 1, caml_alloc_array(&val_option_node, (char const ** )values));
    CAMLreturn(ml_some(tuple));
  }
}

ML_2 (librdf_query_results_get_binding_value, QR_val, Int_val, Val_option_librdf_node)
ML_2 (librdf_query_results_get_binding_name, QR_val, Int_val, Val_option_const_string)
ML_2 (librdf_query_results_get_binding_value_by_name, QR_val, String_val, Val_option_librdf_node)
ML_5 (librdf_query_results_to_string2,  QR_val,
      String_option_val, String_option_val, Librdf_uri_option_val, Librdf_uri_option_val,
      Val_option_ustring_free_memory)

ML_6 (librdf_query_results_to_file_handle2, QR_val,
      File_val, String_option_val, String_option_val,
      Librdf_uri_option_val, Librdf_uri_option_val, Val_int)
ML_bc6 (ml_librdf_query_results_to_file_handle2)

ML_1 (librdf_query_results_is_bindings, QR_val, Val_bool)
ML_1 (librdf_query_results_is_boolean, QR_val, Val_bool)
ML_1 (librdf_query_results_is_graph, QR_val, Val_bool)
ML_1 (librdf_query_results_is_syntax, QR_val, Val_bool)
ML_1 (librdf_query_results_get_boolean, QR_val, Val_int)
