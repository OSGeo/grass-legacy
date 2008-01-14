/**
   \file vertex.cpp

   \brief Vertex manipulation

   This program is free software under the GNU General Public
   License (>=v2). Read the file COPYING that comes with GRASS
   for details.

   (C) 2008 by The GRASS development team

   \author Martin Landa <landa.martin gmail.com>

   \date 2008 
*/

extern "C" {
#include <grass/vedit.h>
}
#include "driver.h"
#include "digit.h"

/**
   \brief Move vertex

   \param x,y,z coordinates (z is used only if map is 3d)
   \param move_x,move_y,move_z direction for moving vertex
   \param snap snap mode (see vector/v.edit/lib/vedit.h)
   \param thresh threshold value to identify vertex position

   \param 1 vertex added/removed
   \param 0 nothing changed
   \param -1 error
*/
int Digit::MoveVertex(double x, double y, double z,
		      double move_x, double move_y, double move_z,
		      int snap, double thresh) {

    int ret;
    struct line_pnts *point;

    if (!display->mapInfo)
	return -1;

    point = Vect_new_line_struct();
    Vect_append_point(point, x, y, z);

    /* move only first found vertex in bbox */
    ret = Vedit_move_vertex(display->mapInfo, NULL, 0, /* TODO: BG */
			    display->selected,
			    point, thresh,
			    move_x, move_y, move_z,
			    1, (snap == SNAPVERTEX) ? 1 : 0); 

    Vect_destroy_line_struct(point);

    return ret;
}

/**
   \brief Add or remove vertex

   Shape of line/boundary is not changed when adding new vertex.
   
   \param add add or remove vertex?
   \param x,y,z coordinates (z is used only if map is 3d
   \param thresh threshold value to identify vertex position

   \param 1 vertex added/removed
   \param 0 nothing changed
   \param -1 error
*/
int Digit::ModifyLineVertex(int add, double x, double y, double z,
			    double thresh)
{
    int ret;
    struct line_pnts *point;

    if (!display->mapInfo || display->selected->n_values != 1)
	return -1;

    point = Vect_new_line_struct();
    Vect_append_point(point, x, y, z);

    if (add) {
	ret = Vedit_add_vertex(display->mapInfo, display->selected,
			       point, thresh);
    }
    else {
	ret = Vedit_remove_vertex(display->mapInfo, display->selected,
				  point, thresh);
    }

    Vect_destroy_line_struct(point);

    return ret;
}
