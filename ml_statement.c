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


#include "ml_statement.h"


Make_Val_final_pointer(librdf_statement, Ignore, Ignore, 0)

ML_1 (librdf_new_statement, Librdf_world_val, Val_option_librdf_statement)
ML_1 (librdf_free_statement, Librdf_statement_val, Unit)
ML_1 (librdf_new_statement_from_statement, Librdf_statement_val, Val_option_librdf_statement)

ML_4 (librdf_new_statement_from_nodes,
      Librdf_world_val, Librdf_node_val, Librdf_node_val, Librdf_node_val, Val_option_librdf_statement)
ML_2 (librdf_statement_init, Librdf_world_val, Librdf_statement_val, Unit)
ML_1 (librdf_statement_clear, Librdf_statement_val, Unit)

ML_1 (librdf_statement_get_subject, Librdf_statement_val, Val_librdf_node)
ML_2 (librdf_statement_set_subject, Librdf_statement_val, Librdf_node_val, Unit)

ML_1 (librdf_statement_get_predicate, Librdf_statement_val, Val_librdf_node)
ML_2 (librdf_statement_set_predicate, Librdf_statement_val, Librdf_node_val, Unit)

ML_1 (librdf_statement_get_object, Librdf_statement_val, Val_librdf_node)
ML_2 (librdf_statement_set_object, Librdf_statement_val, Librdf_node_val, Unit)

ML_1 (librdf_statement_is_complete, Librdf_statement_val, Val_bool)
ML_2 (librdf_statement_print, Librdf_statement_val, File_val, Unit)

ML_2 (librdf_statement_equals, Librdf_statement_val, Librdf_statement_val, Val_bool)
ML_2 (librdf_statement_match, Librdf_statement_val, Librdf_statement_val, Val_bool)
/*
size_t              librdf_statement_encode             (librdf_statement *statement,
                                                         unsigned char *buffer,
                                                         size_t length);
size_t              librdf_statement_encode2            (librdf_world *world,
                                                         librdf_statement *statement,
                                                         unsigned char *buffer,
                                                         size_t length);
size_t              librdf_statement_encode_parts       (librdf_statement *statement,
                                                         librdf_node *context_node,
                                                         unsigned char *buffer,
                                                         size_t length,
                                                         librdf_statement_part fields);
size_t              librdf_statement_encode_parts2      (librdf_world *world,
                                                         librdf_statement *statement,
                                                         librdf_node *context_node,
                                                         unsigned char *buffer,
                                                         size_t length,
                                                         librdf_statement_part fields);
size_t              librdf_statement_decode             (librdf_statement *statement,
                                                         unsigned char *buffer,
                                                         size_t length);
size_t              librdf_statement_decode2            (librdf_world *world,
                                                         librdf_statement *statement,
                                                         librdf_node **context_node,
                                                         unsigned char *buffer,
                                                         size_t length);
size_t              librdf_statement_decode_parts       (librdf_statement *statement,
                                                         librdf_node **context_node,
                                                         unsigned char *buffer,
                                                         size_t length);
int                 librdf_statement_write              (librdf_statement *statement,
                                                         raptor_iostream *iostr);
*/


