#include <stdio.h>
#include "gis.h"
#include "G3d.h"
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_extract2dRegion (region3d, region2d)

     G3D_Region *region3d;
     struct Cell_head *region2d;

{
  region2d->proj = region3d->proj;
  region2d->zone = region3d->zone;

  region2d->north = region3d->north;
  region2d->south = region3d->south;
  region2d->east = region3d->east;
  region2d->west = region3d->west;

  region2d->rows = region3d->rows;
  region2d->cols = region3d->cols;

  region2d->ns_res = region3d->ns_res;
  region2d->ew_res = region3d->ew_res;
}

/*---------------------------------------------------------------------------*/

void
G3d_incorporate2dRegion (region2d, region3d)

     struct Cell_head *region2d;
     G3D_Region *region3d;

{
  region3d->proj = region2d->proj;
  region3d->zone = region2d->zone;

  region3d->north = region2d->north;
  region3d->south = region2d->south;
  region3d->east = region2d->east;
  region3d->west = region2d->west;

  region3d->rows = region2d->rows;
  region3d->cols = region2d->cols;

  region3d->ns_res = region2d->ns_res;
  region3d->ew_res = region2d->ew_res;
}

/*---------------------------------------------------------------------------*/

void
G3d_adjustRegion (region)

     G3D_Region *region;

{
  struct Cell_head region2d;

  G3d_extract2dRegion (region, &region2d);
  G_adjust_Cell_head (&region2d, 1, 1);
  G3d_incorporate2dRegion (&region2d, region);

  if (region->depths <= 0)
    G3d_fatalError ("G3d_adjustRegion: depths <= 0");
  region->tb_res = (region->top  - region->bottom) / region->depths;
}

/*---------------------------------------------------------------------------*/

void
G3d_adjustRegionRes (region)

     G3D_Region *region;

{
  struct Cell_head region2d;

  G3d_extract2dRegion (region, &region2d);
  G_adjust_Cell_head (&region2d, 0, 0);
  G3d_incorporate2dRegion (&region2d, region);

  if (region->tb_res <= 0)
    G3d_fatalError ("G3d_adjustRegionRes: tb_res <= 0");

  region->depths = (region->top  - region->bottom + region->tb_res/2.0) / 
                   region->tb_res;
  if (region->depths == 0) region->depths = 1;
}

/*---------------------------------------------------------------------------*/

void
G3d_regionCopy (regionDest, regionSrc)

     G3D_Region *regionDest, *regionSrc;

{
  G_copy (regionDest, regionSrc, sizeof (G3D_Region));
}

/*---------------------------------------------------------------------------*/

void
G3d_getRegionValue (map, north, east, top, value, type)

     G3D_Map *map;
     double north, east, top;
     char *value;
     int type;

{
  int row, col, depth;

  /* convert (north, east, top) into (row, col, depth) */

  row = map->region.rows - 
        (north - map->region.south) / (map->region.north - map->region.south)*
        map->region.rows;
  col = (east - map->region.west) / (map->region.east - map->region.west)*
        map->region.cols;
  depth = (top - map->region.bottom) / (map->region.top - map->region.bottom)*
	  map->region.depths;

  /* if (row, col, depth) outside window return NULL value */
  if ((row < 0) || (row >= map->region.rows) ||
      (col < 0) || (col >= map->region.cols) ||
      (depth < 0) || (depth >= map->region.depths)) {
    G3d_setNullValue (value, 1, type);
    return;
  }

  /* get value */
  map->resampleFun (map, row, col, depth, value, type);
}

/*---------------------------------------------------------------------------*/

int
G3d_readRegionMap (name, mapset, region)

     char *name, *mapset;
     G3D_Region *region;

{
  char fullName[1000];
  char xname[512], xmapset[512];

  if (G__name_is_fully_qualified (name, xname, xmapset)) 
    G3d_filename (fullName, G3D_HEADER_ELEMENT, xname, mapset);
  else
    G3d_filename (fullName, G3D_HEADER_ELEMENT, name, mapset);
  return G3d_readWindow (region, fullName);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
