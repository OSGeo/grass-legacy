
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_range_updateFromTile (map, tile, rows, cols, depths,
			  xRedundant, yRedundant, zRedundant, nofNum, type)

     G3D_Map *map; 
     char *tile;
     int rows, cols, depths, xRedundant, yRedundant, zRedundant, nofNum;
     int type;

{
  int y, z, cellType;
  struct FPRange *range;

  range = &(map->range);
  cellType = G3d_g3dType2cellType (type);

  if (nofNum == map->tileSize) {
    G_row_update_fp_range (tile, map->tileSize, range, cellType);
    return;
  }

  if (xRedundant) {
    for (z = 0; z < depths; z++) {
      for (y = 0; y < cols; y++) {
	G_row_update_fp_range (tile, rows, range, cellType);
	tile += map->tileX * G3d_length (type);
      }
      if (yRedundant) tile += map->tileX * yRedundant * G3d_length (type);
    }
    return;
  }

  if (yRedundant) {
    for (z = 0; z < depths; z++) {
      G_row_update_fp_range (tile, map->tileX * cols, range, cellType);
      tile += map->tileXY * G3d_length (type);
    }
    return;
  }

  G_row_update_fp_range (tile, map->tileXY * depths, range, cellType);
}

/*---------------------------------------------------------------------------*/

int
G3d_range_load (map) 

     G3D_Map *map;

{
  if (map->operation == G3D_WRITE_DATA) return 1;
  if (G_read_fp_range (map->fileName, map->mapset, &(map->range)) == -1) {
    return 0;
  }

  return 1;
}

/*---------------------------------------------------------------------------*/

void
G3d_range_min_max (map, min, max)

     G3D_Map *map;
     double *min, *max;

{
  G_get_fp_range_min_max (&(map->range), min, max);
}

/*---------------------------------------------------------------------------*/

int
G3d_range_write (map)

     G3D_Map *map;

{
  char path[4096], element[100];

  sprintf(element, "cell_misc/%s", map->fileName);
  G__file_name (path, element, "range", map->mapset);
  unlink(path);

  if (G_write_fp_range (map->fileName, &(map->range)) == -1) {
    G3d_error ("G3d_closeCellNew: error in G_write_fp_range");
    return 0;
  }

  return 1;
}

/*---------------------------------------------------------------------------*/

int
G3d_range_init (map)

     G3D_Map *map;

{
  G_init_fp_range (&(map->range));
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
