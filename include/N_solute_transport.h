
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      solute transport in porous media
* 		part of the gpde library
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include "N_pde.h"

#ifndef _N_SOLUTE_TRANSPORT_H_
#define _N_SOLUTE_TRANSPORT_H_


typedef struct
{
  N_array_3d *c;		/*concentration */
  N_array_3d *c_start;		/*concentration start conditions */
  N_array_3d *diff_x;		/*x part of the diffusion tensor */
  N_array_3d *diff_y;		/*y part of the diffusion tensor */
  N_array_3d *diff_z;		/*z part of the diffusion tensor */
  N_array_3d *q;		/*concentration sources and sinks */
  N_array_3d *R;		/*retardation */
  N_array_3d *cin;		/*concentration  */

  N_array_3d *vx;		/*velocity vector in x direction for each boundary */
  N_array_3d *vy;		/*velocity vector in y direction for each boundary */
  N_array_3d *vz;		/*velocity vector in z direction for each boundary */

  N_array_3d *status;		/*active/inactive/dirichlet cell status */

  double dt;			/*calculation time */
  double ax, ay, az;		/*dispersivity length */

} N_solute_transport_data3d;


typedef struct
{
  N_array_3d *c;		/*concentration */
  N_array_3d *c_start;		/*concentration start conditions */
  N_array_3d *diff_x;		/*x part of the diffusion tensor */
  N_array_3d *diff_y;		/*y part of the diffusion tensor */
  N_array_3d *q;		/*concentration sources and sinks */
  N_array_3d *R;		/*retardation */
  N_array_3d *cin;		/*concentration  */

  N_array_3d *vx;		/*velocity vector in x direction  */
  N_array_3d *vy;		/*velocity vector in y direction  */

  N_array_3d *status;		/*active/inactive/dirichlet cell status */

  double dt;			/*calculation time */
  double ax, ay, az;		/*dispersivity length */

} N_solute_transport_data2d;


inline N_les_row_entries *N_callback_solute_transport_3d (void *solutedata, N_geom_data * geom, int depth, int row,
							  int col);
inline N_les_row_entries *N_callback_solute_transport_2d (void *solutedata, N_geom_data * geom, int row, int col);
N_solute_transport_data3d *N_alloc_solute_transport_data3d (int depths, int rows, int cols);
N_solute_transport_data2d *N_alloc_solute_transport_data2d (int rows, int cols);
void N_free_solute_transport_data3d (N_solute_transport_data3d * data);
void N_free_solute_transport_data2d (N_solute_transport_data2d * data);

#endif
