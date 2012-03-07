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

#include "ml_types.h"
#include "ml_enums.c"

Make_Val_final_pointer(void, Ignore, Ignore, 0)

Make_Val_option(void)
Make_Val_option(raptor_world)
Make_Val_option(raptor_iostream)
Make_Val_option(rasqal_world)
Make_Val_option(librdf_parser)
Make_Val_option(librdf_world)
Make_Val_option(librdf_model)
Make_Val_option(librdf_node)
Make_Val_option(librdf_hash)
Make_Val_option(librdf_query)
Make_Val_option(librdf_query_results)
Make_Val_option(librdf_iterator)
Make_Val_option(librdf_statement)
Make_Val_option(librdf_storage)
Make_Val_option(librdf_storage_factory)
Make_Val_option(librdf_uri)
Make_Val_option(librdf_stream)
