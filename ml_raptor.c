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

#include "ml_raptor.h"


Make_Val_final_pointer(raptor_world, Ignore, Ignore, 0)
Make_Val_final_pointer(raptor_iostream, Ignore, Ignore, 0)

ML_0 (raptor_new_world, Val_option_raptor_world)
ML_1 (raptor_free_world, Raptor_world_val, Unit)

ML_1 (raptor_world_open, Raptor_world_val, Val_int)

ML_5 (raptor_world_guess_parser_name, Raptor_world_val,
       Librdf_uri_option_val,
       String_option_val,
       SizedUString_option_val,
       UString_option_val,
       Val_option_string)



ML_1 (raptor_free_iostream, Raptor_iostream_val, Unit)
ML_2 (raptor_new_iostream_to_file_handle,
      Raptor_world_val, File_val, Val_option_raptor_iostream)

