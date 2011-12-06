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
#include "ml_rasqal.h"



Make_Val_final_pointer(rasqal_world, Ignore, Ignore, 0)

ML_0 (rasqal_new_world, Val_option_rasqal_world)
ML_1 (rasqal_free_world, Rasqal_world_val, Unit)

ML_1 (rasqal_world_open, Rasqal_world_val, Val_int)
ML_2 (rasqal_world_set_raptor, Rasqal_world_val, Raptor_world_option_val, Unit)
ML_1 (rasqal_world_get_raptor, Rasqal_world_val, Val_option_raptor_world)
