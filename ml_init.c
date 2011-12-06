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

#include <librdf.h>
#include "wrappers.h"
#include "ml_init.h"
#include "ml_rasqal.h"



Make_Val_final_pointer(librdf_world, Ignore, Ignore, 0)

ML_0 (librdf_new_world, Val_option_librdf_world)
ML_1 (librdf_free_world, Librdf_world_val, Unit)
ML_1 (librdf_world_open, Librdf_world_val, Unit)
ML_2 (librdf_world_set_rasqal, Librdf_world_val, Rasqal_world_option_val, Unit)
ML_1 (librdf_world_get_rasqal, Librdf_world_val, Val_option_rasqal_world)
ML_1 (librdf_world_init_mutex, Librdf_world_val, Unit)
ML_2 (librdf_world_set_digest, Librdf_world_val, String_val, Unit)

/*
void                librdf_world_set_error              (librdf_world *world,
                                                         void *user_data,
                                                         librdf_log_level_func error_handler);
void                librdf_world_set_warning            (librdf_world *world,
                                                         void *user_data,
                                                         librdf_log_level_func warning_handler);
void                librdf_world_set_logger             (librdf_world *world,
                                                         void *user_data,
                                                         librdf_log_func log_handler);
#define             LIBRDF_WORLD_FEATURE_GENID_BASE
#define             LIBRDF_WORLD_FEATURE_GENID_COUNTER
librdf_node *       librdf_world_get_feature            (librdf_world *world,
                                                         librdf_uri *feature);
int                 librdf_world_set_feature            (librdf_world *world,
                                                         librdf_uri *feature,
                                                         librdf_node *value);
*/
