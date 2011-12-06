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


#include "ml_stream.h"


Make_Val_final_pointer(librdf_stream, Ignore, Ignore, 0)

/*
ML_2 (librdf_new_hash, Librdf_world_val, String_val, Val_option_librdf_hash)
*/
/*ML_1 (librdf_new_hash_from_hash, Librdf_hash_val, Val_option_librdf_hash)*/
ML_1 (librdf_free_stream, Librdf_stream_val, Unit)
ML_1 (librdf_stream_end, Librdf_stream_val, Val_bool)
ML_1 (librdf_stream_next, Librdf_stream_val, Val_bool)
ML_1 (librdf_stream_get_object, Librdf_stream_val, Val_option_librdf_statement)
ML_1 (librdf_stream_get_context2, Librdf_stream_val, Val_option_librdf_node)