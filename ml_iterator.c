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


#include "ml_iterator.h"


Make_Val_final_pointer(librdf_iterator, Ignore, Ignore, 0)

ML_1 (librdf_free_iterator, Librdf_iterator_val, Unit)
ML_1 (librdf_iterator_end, Librdf_iterator_val, Val_bool)
ML_1 (librdf_iterator_next, Librdf_iterator_val, Val_bool)
ML_1 (librdf_iterator_get_object, Librdf_iterator_val, Val_option_voidstar)
ML_1 (librdf_iterator_get_context, Librdf_iterator_val, Val_option_voidstar)
ML_1 (librdf_iterator_get_key, Librdf_iterator_val, Val_option_voidstar)
ML_1 (librdf_iterator_get_value, Librdf_iterator_val, Val_option_voidstar)
