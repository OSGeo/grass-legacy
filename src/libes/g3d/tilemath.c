#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_tileIndex2tile (map, tileIndex, xTile, yTile, zTile)

     G3D_Map *map; 
     int tileIndex;
     int *xTile, *yTile, *zTile;

{
  int tileIndex2d;

  *zTile = tileIndex / map->nxy;
  tileIndex2d = tileIndex % map->nxy;
  *yTile = tileIndex2d / map->nx;
  *xTile = tileIndex2d % map->nx;
}

/*---------------------------------------------------------------------------*/

int
G3d_tile2tileIndex (map, xTile, yTile, zTile)

     G3D_Map *map; 
     int xTile, yTile, zTile;

{
  return map->nxy * zTile + map->nx * yTile + xTile;
}

/*---------------------------------------------------------------------------*/

void
G3d_tileCoordOrigin (map, xTile, yTile, zTile, x, y, z)

     G3D_Map *map; 
     int xTile, yTile, zTile;
     int *x, *y, *z;

{
  *x = map->tileX * xTile;
  *y = map->tileY * yTile;
  *z = map->tileZ * zTile;
}

/*---------------------------------------------------------------------------*/

void
G3d_tileIndexOrigin (map, tileIndex, x, y, z)

     G3D_Map *map; 
     int tileIndex;
     int *x, *y, *z;

{
  int xTile, yTile, zTile;

  G3d_tileIndex2tile (map, tileIndex, &xTile, &yTile, &zTile);
  G3d_tileCoordOrigin (map, xTile, yTile, zTile, x, y, z);
}

/*---------------------------------------------------------------------------*/

void
G3d_coord2tileCoord (map, x, y, z, xTile, yTile, zTile, xOffs, yOffs, zOffs)

     G3D_Map *map; 
     int x, y, z;
     int *xTile, *yTile, *zTile, *xOffs, *yOffs, *zOffs;

{
  *xTile = x / map->tileX; *xOffs = x % map->tileX;
  *yTile = y / map->tileY; *yOffs = y % map->tileY;
  *zTile = z / map->tileZ; *zOffs = z % map->tileZ;
}

/*---------------------------------------------------------------------------*/

void
G3d_coord2tileIndex (map, x, y, z, tileIndex, offset)

     G3D_Map *map; 
     int x, y, z;
     int *tileIndex, *offset;

{
  int xTile, yTile, zTile, xOffs, yOffs, zOffs;

  G3d_coord2tileCoord (map, x, y, z, 
		       &xTile, &yTile, &zTile, &xOffs, &yOffs, &zOffs);
  *tileIndex = G3d_tile2tileIndex (map, xTile, yTile, zTile);
  *offset = zOffs * map->tileXY + yOffs * map->tileX + xOffs;
}

/*---------------------------------------------------------------------------*/

int 
G3d_coordInRange (map, x, y, z)

     G3D_Map *map; 
     int x, y, z;

{
  return (x >= 0) && (x < map->region.cols) && (y >= 0) && (y < map->region.rows) &&
	 (z >= 0) && (z < map->region.depths);
}

/*---------------------------------------------------------------------------*/

int
G3d_tileIndexInRange (map, tileIndex)

     G3D_Map *map;
     int tileIndex;

{
  return (tileIndex < map->nTiles) && (tileIndex >= 0);
}

/*---------------------------------------------------------------------------*/

int
G3d_tileInRange (map, x, y, z)

     G3D_Map *map;
     int x, y, z;

{
  return (x >= 0) && (x < map->nx) && (y >= 0) && (y < map->ny) && 
         (z >= 0) && (z < map->nz);
}

/*---------------------------------------------------------------------------*/

int
G3d_computeClippedTileDimensions (map, tileIndex,
				  rows, cols, depths,
				  xRedundant, yRedundant, zRedundant)

     G3D_Map *map; 
     int tileIndex;
     int *rows, *cols, *depths, *xRedundant, *yRedundant, *zRedundant;

{
  int x, y, z;

  G3d_tileIndex2tile (map, tileIndex, &x, &y, &z);

  if ((x != map->clipX) && (y != map->clipY) && (z != map->clipZ)) {
    return map->tileSize;
  }

  if (x != map->clipX) {
    *cols = map->tileX; 
    *xRedundant = 0;
  } else {
    *cols = (map->region.cols - 1) % map->tileX + 1;
    *xRedundant = map->tileX - *cols;
  }
  if (y != map->clipY) {
    *rows = map->tileY; 
    *yRedundant = 0;
  } else {
    *rows = (map->region.rows - 1) % map->tileY + 1;
    *yRedundant = map->tileY - *rows;
  }
  if (z != map->clipZ) {
    *depths = map->tileZ; 
    *zRedundant = 0;
  } else {
    *depths = (map->region.depths - 1) % map->tileZ + 1;
    *zRedundant = map->tileZ - *depths;
  }

/* printf ("%d (%d %d %d): (%d %d) (%d %d) (%d %d), %d\n", */
/* 	tileIndex, x, y, z, *rows, *xRedundant, *cols, *yRedundant,  */
/* 	*depths, *zRedundant, *depths * *cols * *rows); */

  return *depths * *cols * *rows;
}

/*---------------------------------------------------------------------------*/

int
G3d_isValidLocation (map, north, east, top)

     G3D_Map *map; 
     double north, east, top;

{
  return ((north >= map->region.south) && (north <= map->region.north) &&
	  (east >= map->region.west) && (east <= map->region.east) &&
	  (((top >= map->region.bottom) && (top <= map->region.top)) ||
	   ((top <= map->region.bottom) && (top >= map->region.top))));
}

/*---------------------------------------------------------------------------*/

void
G3d_location2coord (map, north, east, top, x, y, z)

     G3D_Map *map; 
     double north, east, top;
     int *x, *y, *z;

{
  if (! G3d_isValidLocation (map, north, east, top))
    G3d_fatalError ("location2coord: location not in region");

  *y = (north - map->region.south) / (map->region.north - map->region.south) * 
       (map->region.rows - 1);
  *x = (east - map->region.west) / (map->region.east - map->region.west) * 
        (map->region.cols - 1);
  *z = (top - map->region.bottom) / (map->region.top - map->region.bottom) * 
       (map->region.depths - 1);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
