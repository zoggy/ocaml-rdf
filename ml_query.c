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


#include "ml_query.h"


Make_Val_final_pointer(librdf_query, Ignore, Ignore, 0)

ML_5 (librdf_new_query, Librdf_world_val,
      String_val, Librdf_uri_option_val, UString_val, Librdf_uri_option_val, Val_option_librdf_query)
ML_1 (librdf_free_query, Librdf_query_val, Unit)
ML_1 (librdf_new_query_from_query, Librdf_query_val, Val_option_librdf_query)
