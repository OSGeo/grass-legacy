
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

                       /* EXPORTED FUNCTIONS */

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

char *
G3d_getTilePtr (map, tileIndex)

     G3D_Map *map;
     int tileIndex;
     
{
  char *ptr;

  if ((tileIndex >= map->nTiles) || (tileIndex < 0)) {
    G3d_error ("G3d_getTilePtr: tileIndex out of range");
    return (char *) NULL;
  } 

  if (map->useCache) {
    ptr = G3d_cache_elt_ptr (map->cache, tileIndex);
    if (ptr == NULL) {
      G3d_error ("G3d_getTilePtr: error in G3d_cache_elt_ptr");
      return (char *) NULL;
    } 
    return ptr;
  }

  if (map->currentIndex == tileIndex) return map->data;

  map->currentIndex = tileIndex;
  if (! G3d_readTile (map, map->currentIndex, map->data, map->typeIntern)) {
    G3d_error ("G3d_getTilePtr: error in G3d_readTile");
    return (char *) NULL;
  } 

  return map->data;
}

/*---------------------------------------------------------------------------*/

int
G3d_tileLoad (map, tileIndex)

     G3D_Map *map;
     int tileIndex;
     
{
  if (G3d_getTilePtr (map, tileIndex) == NULL) {
    G3d_error ("G3d_tileLoad: error in G3d_getTilePtr");
    return 0;
  }

  return 1;
}

/*---------------------------------------------------------------------------*/

int
G3d__removeTile (map, tileIndex)

 G3D_Map *map; 
     int tileIndex;

{
  if (! map->useCache) return 1;

  if (! G3d_cache_remove_elt (map->cache, tileIndex)) {
    G3d_error ("G3d_removeTile: error in G3d_cache_remove_elt");
    return 0;
  }

  return 1;
}

/*---------------------------------------------------------------------------*/

double G3d_getDoubleRegion ();

float
G3d_getFloatRegion (map, x, y, z)

     G3D_Map *map;
     int x, y, z;

{
  int tileIndex, offs;
  float *tile;

  if (map->typeIntern == G3D_DOUBLE) 
    return (float) G3d_getDoubleRegion (map, x, y, z);

  G3d_coord2tileIndex (map, x, y, z, &tileIndex, &offs);
  tile = (float *) G3d_getTilePtr (map, tileIndex);

  if (tile == NULL) 
    G3d_fatalError ("G3d_getFloatRegion: error in G3d_getTilePtr");

  return tile[offs];
}

/*---------------------------------------------------------------------------*/

double
G3d_getDoubleRegion (map, x, y, z)

     G3D_Map *map;
     int x, y, z;

{
  int tileIndex, offs;
  double *tile;

  if (map->typeIntern == G3D_FLOAT) 
    return (double) G3d_getFloatRegion (map, x, y, z);

  G3d_coord2tileIndex (map, x, y, z, &tileIndex, &offs);
  tile = (double *) G3d_getTilePtr (map, tileIndex);
  
  if (tile == NULL) 
    G3d_fatalError ("G3d_getDoubleRegion: error in G3d_getTilePtr");

  return tile[offs];
}

/*---------------------------------------------------------------------------*/

void
G3d_getValueRegion (map, x, y, z, value, type)

     G3D_Map *map;
     int x, y, z;
     char *value;
     int type;

{
  if (type == G3D_FLOAT) {
    *((float *) value) = G3d_getFloatRegion (map, x, y, z);
    return;
  }

  *((double *) value) = G3d_getDoubleRegion (map, x, y, z);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
