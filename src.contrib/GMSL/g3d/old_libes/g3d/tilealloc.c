
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

char *
G3d_allocTilesType (map, nofTiles, type)

     G3D_Map *map;
     int nofTiles, type;

{
  char *tiles;

  tiles = G3d_malloc (map->tileSize * G3d_length (type) * nofTiles);
  if (tiles == NULL) {
    G3d_error ("G3d_allocTilesType: error in G3d_malloc");
    return (char *) NULL;
  }

  return tiles;
}

/*---------------------------------------------------------------------------*/

char *
G3d_allocTiles (map, nofTiles)

     G3D_Map *map;
     int nofTiles;

{
  char *tiles;

  tiles = G3d_allocTilesType (map, nofTiles, map->typeIntern);
  if (tiles == NULL) {
    G3d_error ("G3d_allocTiles: error in G3d_allocTilesType");
    return (char *) NULL;
  }

  return tiles;
}

/*---------------------------------------------------------------------------*/

void
G3d_freeTiles (tiles)

     char *tiles;

{
  G3d_free (tiles);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
