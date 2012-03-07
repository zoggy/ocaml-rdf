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

#include "ml_uri.h"


Make_Val_final_pointer(librdf_uri, Ignore, Ignore, 0)

ML_2 (librdf_new_uri, Librdf_world_val, UString_val, Val_option_librdf_uri)
ML_3 (librdf_new_uri2, Librdf_world_val, UString_val, Int_val, Val_option_librdf_uri)

ML_1 (librdf_new_uri_from_uri, Librdf_uri_val, Val_option_librdf_uri)
ML_2 (librdf_new_uri_from_uri_local_name, Librdf_uri_val, UString_val, Val_option_librdf_uri)

ML_1 (librdf_free_uri, Librdf_uri_val, Unit)

ML_1 (librdf_uri_as_string, Librdf_uri_val, Val_ustring)

ML_2 (librdf_uri_equals, Librdf_uri_val, Librdf_uri_val, Val_bool)
ML_1 (librdf_uri_is_file_uri, Librdf_uri_val, Val_bool)
ML_1 (librdf_uri_to_filename, Librdf_uri_val, Val_option_const_string)

ML_3 (librdf_new_uri_normalised_to_base,
      UString_val, Librdf_uri_val, Librdf_uri_val, Val_option_librdf_uri)
ML_2 (librdf_new_uri_relative_to_base,
      Librdf_uri_val, UString_val, Val_option_librdf_uri)
ML_2 (librdf_new_uri_from_filename, Librdf_world_val, String_val, Val_option_librdf_uri)

ML_2 (librdf_uri_compare, Librdf_uri_val, Librdf_uri_val, Val_int)
/*
int                 (*librdf_uri_filter_func)           (void *user_data,
                                                         librdf_uri *uri);
*/