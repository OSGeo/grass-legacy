#include <stdio.h>
#include "gis.h"
#include "G3d.h"
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

G3D_Region g3d_window;

/*---------------------------------------------------------------------------*/

void
G3d_setWindowMap (map, window)

     G3D_Map *map;
     G3D_Region *window;

{
  G3d_regionCopy (&(map->window), window);
  G3d_adjustRegion (&(map->window));
}

/*---------------------------------------------------------------------------*/

void
G3d_setWindow (window)

     G3D_Region *window;

{
  G3d_regionCopy (&g3d_window, window);
  G3d_adjustRegion (&g3d_window);
}

/*---------------------------------------------------------------------------*/

void
G3d_getWindow (window)

     G3D_Region *window;

{
  G3d_regionCopy (window, &g3d_window);
}

/*---------------------------------------------------------------------------*/

G3D_Region *
G3d_windowPtr ()

{
  return &g3d_window;
}

/*---------------------------------------------------------------------------*/

void
G3d_getValue (map, x, y, z, value, type)

     G3D_Map *map;
     int x, y, z;
     char *value;
     int type;

{
  double north, east, top;

/*AV*/
/* BEGIN OF ORIGINAL CODE */
/*
 int row, col, depth;
*/
/* END OF ORIGINAL CODE */

/*AV*/
/* BEGIN OF MY CODE */
	double row, col, depth;
/* END OF MY CODE */

  /* convert (x, y, z) into (north, east, top) */

/*AV*/
/* BEGIN OF ORIGINAL CODE */
/*
	north = ((double) map->window.rows - y - 0.5) / (double) map->window.rows *
          (map->window.north - map->window.south) + map->window.south;
*/
/* END OF ORIGINAL CODE */

/*AV*/
/* BEGIN OF MY CODE */
  north = ((double) y + 0.5) / (double) map->window.rows *
          (map->window.north - map->window.south) + map->window.south;
/* END OF MY CODE */

  east = ((double) x + 0.5) / (double) map->window.cols *
          (map->window.east - map->window.west) + map->window.west;
  top = ((double) z + 0.5) / (double) map->window.depths *
          (map->window.top - map->window.bottom) + map->window.bottom;

  /* convert (north, east, top) into (row, col, depth) */

/*AV*/
/* BEGIN OF ORIGINAL CODE */
/*
	row = map->region.rows -
        (north - map->region.south) / (map->region.north - map->region.south) *
        map->region.rows;
*/
/* END OF ORIGINAL CODE */

/*AV*/
/* BEGIN OF MY CODE */
	row = (north - map->region.south) / (map->region.north - map->region.south) *
        map->region.rows;
/* END OF MY CODE */

	col = (east - map->region.west) / (map->region.east - map->region.west) *
        map->region.cols;
  depth = (top - map->region.bottom) / (map->region.top - map->region.bottom) *
	  		map->region.depths;

  /* if (row, col, depth) outside window return NULL value */
	if ((row < 0) || (row >= map->region.rows) ||
      (col < 0) || (col >= map->region.cols) ||
      (depth < 0) || (depth >= map->region.depths)) {
    G3d_setNullValue (value, 1, type);
    return;
	}

  /* get value */
  map->resampleFun (map, (int)row, (int)col, (int)depth, value, type);

}

/*---------------------------------------------------------------------------*/

float
G3d_getFloat (map, x, y, z)

     G3D_Map *map;
     int x, y, z;

{
  float value;

  G3d_getValue (map, x, y, z, (char *)&value, G3D_FLOAT);
  return value;
}

/*---------------------------------------------------------------------------*/

double
G3d_getDouble (map, x, y, z)

     G3D_Map *map;
     int x, y, z;

{
  double value;

  G3d_getValue (map, x, y, z, (char *)&value, G3D_DOUBLE);
  return value;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
