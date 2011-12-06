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


#include "ml_hash.h"


Make_Val_final_pointer(librdf_hash, Ignore, Ignore, 0)


/*
ML_2 (librdf_new_hash, Librdf_world_val, String_val, Val_option_librdf_hash)
*/

ML_1 (librdf_new_hash_from_hash, Librdf_hash_val, Val_option_librdf_hash)
ML_3 (librdf_new_hash_from_string, Librdf_world_val, String_val, String_val, Val_option_librdf_hash)
ML_1 (librdf_free_hash, Librdf_hash_val, Unit)
ML_2 (librdf_hash_get, Librdf_hash_val, String_val, Val_option_string_free)
ML_2 (librdf_hash_get_as_boolean, Librdf_hash_val, String_val, Val_int)
ML_2 (librdf_hash_get_as_long, Librdf_hash_val, String_val, Val_long)
ML_2 (librdf_hash_get_del, Librdf_hash_val, String_val, Val_string_free)
ML_3 (librdf_hash_put_strings, Librdf_hash_val, String_val, String_val, Val_int)
ML_4 (librdf_hash_interpret_template, UString_val, Librdf_hash_val, UString_val, UString_val, Val_ustring)
ML_2 (librdf_hash_from_string, Librdf_hash_val, String_val, Val_int)
/*
ML_2 (librdf_hash_to_string, Librdf_hash_val, ???, Val_string_free)
*/
/*
void                librdf_hash_print                   (librdf_hash *hash,
                                                         FILE *fh);
void                librdf_hash_print_keys              (librdf_hash *hash,
                                                         FILE *fh);
void                librdf_hash_print_values            (librdf_hash *hash,
                                                         const char *key_string,
                                                         FILE *fh);
librdf_hash *       librdf_new_hash_from_array_of_strings
                                                        (librdf_world *world,
                                                         const char *name,
                                                         const char **array);
*/
