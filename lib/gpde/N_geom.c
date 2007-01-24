
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      part of the gpde library
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/


#include "grass/N_pde.h"

/* *************************************************************** * 
 * *************************************************************** * 
 * *************************************************************** */
/*!
 * \brief Allocate the pde geometry data structure and return a pointer to the new allocated structure
 *
 * \return N_geom_data *
 * */
inline N_geom_data *
N_alloc_geom_data ()
{
  N_geom_data *geo = (N_geom_data *) G_calloc (1, sizeof (N_geom_data));
  return geo;
}
