
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      groundwater flow in porous media 
* 		part of the gpde library
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#ifndef _N_GWFLOW_H_
#define _N_GWFLOW_H_
#include "N_pde.h"

#define N_GW_CONFINED 0		/*confined groundwater */
#define N_GW_UNCONFINED 1	/*unconfined groundwater */

/*!
 * \brief This data structure contains all data needed to compute the 
 * groundwater mass balance in three dimension 
 * */
typedef struct
{
  N_array_3d *phead;		/*!piezometric head */
  N_array_3d *phead_start;	/*!start conditions */
  N_array_3d *hc_x;		/*!x part of the hydraulic conductivity tensor */
  N_array_3d *hc_y;		/*!y part of the hydraulic conductivity tensor */
  N_array_3d *hc_z;		/*!z part of the hydraulic conductivity tensor */
  N_array_3d *q;		/*!sources and sinks */
  N_array_2d *r;		/*!reacharge at the top of the gw leayer */
  N_array_3d *s;		/*!specific yield */
  N_array_3d *nf;		/*!effective porosity */

  N_array_3d *status;		/*!active/inactive/dirichlet cell status */

  double dt;			/*!calculation time */

} N_gwflow_data3d;

/*!
 * \brief This data structure contains all data needed to compute the 
 * groundwater mass balance in two dimension 
 * */
typedef struct
{
  N_array_2d *phead;		/*!piezometric head */
  N_array_2d *phead_start;	/*!start conditions */
  N_array_2d *hc_x;		/*!x part of the hydraulic conductivity tensor */
  N_array_2d *hc_y;		/*!y part of the hydraulic conductivity tensor */
  N_array_2d *q;		/*!sources and sinks */
  N_array_2d *r;		/*!reacharge at the top of the gw leayer */
  N_array_2d *s;		/*!specific yield */
  N_array_2d *nf;		/*!effective porosity */

  N_array_2d *top;		/*!top surface of the quifer */
  N_array_2d *bottom;		/*!bottom of the aquifer */

  N_array_2d *status;		/*!active/inactive/dirichlet cell status */

  double dt;			/*!calculation time */
  int gwtype;			/*!Which type of groundwater, N_GW_CONFINED or N_GW_UNCONFIED */

} N_gwflow_data2d;

extern N_data_star *N_callback_gwflow_3d (void *gwdata, N_geom_data * geom, int col, int row, int depth);
extern N_data_star *N_callback_gwflow_2d (void *gwdata, N_geom_data * geom, int col, int row);
extern N_gwflow_data3d *N_alloc_gwflow_data3d (int cols, int rows, int depths);
extern N_gwflow_data2d *N_alloc_gwflow_data2d (int cols, int rows);
extern void N_free_gwflow_data3d (N_gwflow_data3d * data);
extern void N_free_gwflow_data2d (N_gwflow_data2d * data);
#endif
