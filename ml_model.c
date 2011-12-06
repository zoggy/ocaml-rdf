/*********************************************************************************/
/*                OCaml-RDF                                                      */
/*                                                                               */
/*    Copyright (C) 2011 Institut National de Recherche en Informatique          */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU Library General Public License version       */
/*    2.1 or later as published by the Free Software Foundation.                 */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU Library General Public          */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*                                                                               */
/*********************************************************************************/

/* */


#include "ml_model.h"


Make_Val_final_pointer(librdf_model, Ignore, Ignore, 0)

ML_3 (librdf_new_model, Librdf_world_val, Librdf_storage_val, String_val, Val_option_librdf_model)
ML_1 (librdf_new_model_from_model, Librdf_model_val, Val_librdf_model)
ML_1 (librdf_free_model, Librdf_model_val, Unit)

ML_2 (librdf_model_add_statement, Librdf_model_val, Librdf_statement_val, Val_int)
ML_2 (librdf_model_add_statements, Librdf_model_val, Librdf_stream_val, Val_int)
ML_2 (librdf_model_remove_statement, Librdf_model_val, Librdf_statement_val, Val_int)
ML_2 (librdf_model_contains_statement, Librdf_model_val, Librdf_statement_val, Val_int)
ML_2 (librdf_model_find_statements, Librdf_model_val, Librdf_statement_val, Val_option_librdf_stream)
ML_4 (librdf_model_find_statements_with_options, Librdf_model_val,
      Librdf_statement_val, Librdf_node_option_val, Librdf_hash_option_val, Val_option_librdf_stream)
ML_3 (librdf_model_get_sources, Librdf_model_val, Librdf_node_val, Librdf_node_val,
      Val_option_librdf_iterator)
ML_3 (librdf_model_get_arcs, Librdf_model_val, Librdf_node_val, Librdf_node_val,
      Val_option_librdf_iterator)
ML_3 (librdf_model_get_targets, Librdf_model_val, Librdf_node_val, Librdf_node_val,
      Val_option_librdf_iterator)

ML_2 (librdf_model_write, Librdf_model_val, Raptor_iostream_val, Val_int)

ML_3 (librdf_model_context_add_statement, Librdf_model_val, Librdf_node_val, Librdf_statement_val, Val_int)
ML_3 (librdf_model_context_add_statements, Librdf_model_val, Librdf_node_val, Librdf_stream_val, Val_int)
ML_3 (librdf_model_context_remove_statement, Librdf_model_val, Librdf_node_val, Librdf_statement_val, Val_int)

ML_3 (librdf_model_find_statements_in_context, Librdf_model_val,
      Librdf_statement_val, Librdf_node_option_val, Val_option_librdf_stream)

ML_1 (librdf_model_get_contexts, Librdf_model_val, Val_option_librdf_iterator)

ML_2 (librdf_model_get_feature, Librdf_model_val, Librdf_uri_val, Val_option_librdf_node)
ML_3 (librdf_model_set_feature, Librdf_model_val, Librdf_uri_val, Librdf_node_val, Val_int)

ML_1 (librdf_model_transaction_commit, Librdf_model_val, Val_int)
ML_1 (librdf_model_transaction_get_handle, Librdf_model_val,  Val_voidstar)
ML_1 (librdf_model_transaction_rollback, Librdf_model_val, Val_int)
ML_1 (librdf_model_transaction_start, Librdf_model_val, Val_int)
ML_2 (librdf_model_transaction_start_with_handle, Librdf_model_val, Voidstar_val, Val_int)
