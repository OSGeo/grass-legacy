#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_setNullTileType (map, tile, type)

     G3D_Map *map; 
     char *tile;
     int type;

{
  G3d_setNullValue (tile, map->tileSize, type);
}

/*---------------------------------------------------------------------------*/

void
G3d_setNullTile (map, tile)

     G3D_Map *map; 
     char *tile;

{
  G3d_setNullTileType (map, tile, map->typeIntern);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
