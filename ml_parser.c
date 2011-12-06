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


#include "ml_parser.h"


Make_Val_final_pointer(librdf_parser, Ignore, Ignore, 0)

ML_4 (librdf_new_parser, Librdf_world_val,
      String_option_val, String_option_val, Librdf_uri_option_val, Val_option_librdf_parser)
ML_1 (librdf_free_parser, Librdf_parser_val, Unit)
ML_4 (librdf_parser_parse_into_model, Librdf_parser_val,
       Librdf_uri_val, Librdf_uri_option_val, Librdf_model_val, Val_int)
ML_4 (librdf_parser_parse_string_into_model, Librdf_parser_val,
       UString_val, Librdf_uri_option_val, Librdf_model_val, Val_int)       