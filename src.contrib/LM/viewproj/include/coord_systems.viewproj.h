/*
****************************************************************************
*
* MODULE:       coord_systems.viewproj.h
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To declare viewproj stuctures and library functions.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
******************************************************************************
coord_systems.viewproj.h

This file contains structures for points in these 4 coordinate systems.

a) screen	-  the display window  (in pixels)
b) cartPROJ	-  the cartesian system the PROJ uses by default
c) latlon	-  the world coordinates
d) map		-  the row & column of the CELL in the raster-data-file

It also contains routines for transforming between the coordinate systems.

USING THESE ROUTINES:
First call setup_conversions_viewproj(). 
Then all the other functions can be used.

To free the memory used by PROJ, call free_conversions_viewproj();

Sharif Razzaque June 1995
*************************************************************************
*/


# define INVALID_COORD -1
# define VALID_COORD 1

typedef struct
{
	short x; /* since screen coords never exceed 65536 */
	short y;
} screen_coord_type;

typedef struct
{
	double x; /* as mandated by PROJ.4 */
	double y;
} cartPROJ_coord_type; /* the 'unscaled' cartisian system that PROJ uses*/

typedef struct
{
	double lat; /* as mandated by PROJ.4 */
	double lon;
	int valid;
} latlon_coord_type;

typedef struct
{
	int col; /* as mandated by GRASS code */
	int row;
} map_coord_type;


/*
coord_system transformations_______________________________________________
*/

extern int setup_conversions_viewproj (int draw_gird, int *t_out, int *b_out, 
					int *l_out, int *r_out);
 /* call before doing any conversions 
    returns the number of rows in the file data 
    if draw_grid is TRUE, it draws a grid of points before drawing the map
*/

extern void free_conversions_viewproj (void); /*call after done with conversions */

extern cartPROJ_coord_type latlon_to_cartPROJ (latlon_coord_type from);

extern latlon_coord_type cartPROJ_to_latlon (cartPROJ_coord_type from);
/* returns .valid=INVALID_COORD if point is not inverse projectable to latlon*/

extern screen_coord_type cartPROJ_to_screen (cartPROJ_coord_type from);
extern cartPROJ_coord_type screen_to_cartPROJ (screen_coord_type from);

extern map_coord_type latlon_to_map (latlon_coord_type from);
/* returns INVALID_COORD if point not in selected area */

extern latlon_coord_type screen_to_latlon (screen_coord_type from);
extern map_coord_type screen_to_map (screen_coord_type from);

extern screen_coord_type latlon_to_screen (latlon_coord_type from);



