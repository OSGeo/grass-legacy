
#include <stdio.h>
#include "gis.h"
#include "G3d_intern.h"

/*--------------------------------------------------------------------------*/

/* the standard g3d file format is used to store the mask values. a NULL-value
   is stored for values which are masked out and a "0." is stored for values 
   which are not masked out. to improve compression, the precision is set to 
   0 and RLE encoding is used.
*/

/*--------------------------------------------------------------------------*/

static int G3d_maskMapExistsVar = 0;
static G3D_Map *G3d_maskMap;

/*--------------------------------------------------------------------------*/

static float G3D_MASKNUMmaskValue;

#define G3D_MASKNUM(map,Xmask,Ymask,Zmask,VALUEmask,TYPEmask) \
\
   (G3D_MASKNUMmaskValue = G3d_getMaskFloat (map, Xmask, Ymask, Zmask), \
    ((G3d_isNullValueNum (&G3D_MASKNUMmaskValue, G3D_FLOAT)) ? \
      G3d_setNullValue (VALUEmask, 1, TYPEmask) : NULL))

/*--------------------------------------------------------------------------*/

int
G3d_maskClose ()

{
  /* No Idea if this is correct return value */
  if (! G3d_maskMapExistsVar) return 1;  

  G3d_maskMapExistsVar = 0;

  if (! G3d_closeCell (G3d_maskMap)) {
    G3d_error ("G3d_maskClose: error closing mask");

    return 0;
  }

  return 1;
}

/*--------------------------------------------------------------------------*/

int
G3d_maskFileExists ()

{
  char buf[200];

  sprintf (buf, "%s/%s", G3D_DIRECTORY, G3D_MASK_MAP);
  return (G_find_file (buf, G3D_CELL_ELEMENT, G_mapset ()) != NULL);
}

/*--------------------------------------------------------------------------*/

static int maskOpenOldCacheDefault = G3D_USE_CACHE_DEFAULT;

int
G3d_maskOpenOld ()

{
  double min, max;
  G3D_Region region;

  /* No Idea if this is correct return value */
  if (G3d_maskMapExistsVar) return 1;

  G3d_maskMapExistsVar = G3d_maskFileExists ();

  if (! G3d_maskMapExistsVar) return 1;

  if ((G3d_maskMap = G3d_openCellOld (G3D_MASK_MAP, G_mapset (), 
				      G3D_DEFAULT_WINDOW, G3D_FLOAT, 
				      maskOpenOldCacheDefault))
      == NULL) {
    G3d_error ("G3d_maskOpenOld: cannot open mask");

    return 0;
  }

  G3d_getRegionStructMap (G3d_maskMap, &region);
  G3d_setWindowMap (G3d_maskMap, &region);

  return 1;
} 

/*--------------------------------------------------------------------------*/

static float
G3d_getMaskFloat (map, x, y, z)

     G3D_Map *map; 
     int x, y, z;

{
  double north, east, top;
  float value;

  north = ((double) map->window.rows - y - 0.5) / (double) map->window.rows * 
          (map->window.north - map->window.south) + map->window.south;
  east = ((double) x + 0.5) / (double) map->window.cols * 
          (map->window.east - map->window.west) + map->window.west;
  top = ((double) z + 0.5) / (double) map->window.depths * 
          (map->window.top - map->window.bottom) + map->window.bottom;

  G3d_getRegionValue (G3d_maskMap, north, east, top, (char *)&value, G3D_FLOAT);
  return value;
}     

/*--------------------------------------------------------------------------*/

int
G3d_maskReopen (cache)

     int cache;

{
  int tmp;

  if (G3d_maskMapExistsVar) 
    if (! G3d_maskClose ()) {
      G3d_error ("G3d_maskReopen: error closing mask");

      return 0;
    }

  tmp = maskOpenOldCacheDefault;
  maskOpenOldCacheDefault = cache;

  if (! G3d_maskOpenOld ())  {
    G3d_error ("G3d_maskReopen: error opening mask");

    return 0;
  }

  maskOpenOldCacheDefault = tmp;
  return 1;
}

/*--------------------------------------------------------------------------*/

int
G3d_isMasked (map, x, y, z)

     G3D_Map *map; 
     int x, y, z;

{
  if (! G3d_maskMapExistsVar) return 0;

  G3D_MASKNUMmaskValue = G3d_getMaskFloat (map, x, y, z);
  return (G3d_isNullValueNum (&G3D_MASKNUMmaskValue, G3D_FLOAT));
}

/*--------------------------------------------------------------------------*/

void
G3d_maskNum (map, x, y, z, value, type)

     G3D_Map *map; 
     int x, y, z;
     void *value;
     int type;

{
  if (! G3d_maskMapExistsVar) return;
  G3D_MASKNUM (map, x, y, z, value, type);
}

/*--------------------------------------------------------------------------*/

void
G3d_maskFloat (map, x, y, z, value)

     G3D_Map *map; 
     int x, y, z;
     float *value;

{
  if (! G3d_maskMapExistsVar) return;
  G3D_MASKNUM (map, x, y, z, value, G3D_FLOAT);
}

/*--------------------------------------------------------------------------*/

void
G3d_maskDouble (map, x, y, z, value)

     G3D_Map *map; 
     int x, y, z;
     double *value;

{
  if (! G3d_maskMapExistsVar) return;
  G3D_MASKNUM (map, x, y, z, value, G3D_DOUBLE);
}

/*--------------------------------------------------------------------------*/

void
G3d_maskTile (map, tileIndex, tile, type)

     G3D_Map *map; 
     int tileIndex;
     char *tile;
     int type;

{
  int nofNum, rows, cols, depths, xRedundant, yRedundant, zRedundant;
  int x, y, z, xLength, yLength, dx, dy, dz, length;

  if (! G3d_maskMapExistsVar) return;

  nofNum = G3d_computeClippedTileDimensions (map, tileIndex, 
					     &rows, &cols, &depths,
					     &xRedundant, &yRedundant, 
					     &zRedundant);
  G3d_tileIndexOrigin (map, tileIndex, &x, &y, &z);

  if (nofNum == map->tileSize) {
    G3d_getTileDimensionsMap (map, &rows, &cols, &depths);
    xRedundant = yRedundant = 0;
  }

  rows += y;
  cols += x;
  depths += z;
  length = G3d_length (type);
  xLength = xRedundant * length;
  yLength = map->tileX * yRedundant * length;

  for (dz = z; dz < depths; dz++) {
    for (dy = y; dy < rows; dy++) {
      for (dx = x; dx < cols; dx++) {
	G3D_MASKNUM (map, dx, dy, dz, tile, type);
	tile += length;
      }

      tile += xLength;
    }
    tile += yLength;
  }
}

/*--------------------------------------------------------------------------*/

void
G3d_maskOn (map) G3D_Map *map; { map->useMask = 1; }

void 
G3d_maskOff (map) G3D_Map *map; { map->useMask = 0; }

int
G3d_maskIsOn (map) G3D_Map *map; { return map->useMask; }

int
G3d_maskIsOff (map) G3D_Map *map; { return ! map->useMask; }

char * 
G3d_maskFile () { return G3D_MASK_MAP; }

int
G3d_maskMapExists () { return G3d_maskMapExistsVar; }

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

