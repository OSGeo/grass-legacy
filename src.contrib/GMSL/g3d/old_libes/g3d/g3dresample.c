#include <stdio.h>
#include "gis.h"
#include "G3d_intern.h"

/*--------------------------------------------------------------------------*/

void
G3d_nearestNeighbor (map, row, col, depth, value, type)

     G3D_Map *map;
     int row, col, depth;
     char *value;
     int type;

{
  G3d_getValueRegion (map, row, col, depth, value, type);
}

/*--------------------------------------------------------------------------*/

void
G3d_setResamplingFun (map, resampleFun)

     G3D_Map *map;
     void (*resampleFun) ();

{
  map->resampleFun = resampleFun;
}

/*--------------------------------------------------------------------------*/

void
G3d_getResamplingFun (map, resampleFun)

     G3D_Map *map;
     void (**resampleFun) ();

{
  *resampleFun = map->resampleFun;
}

/*--------------------------------------------------------------------------*/

void
G3d_getNearestNeighborFunPtr (nnFunPtr)

     void (**nnFunPtr) ();

{
  *nnFunPtr = G3d_nearestNeighbor;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
