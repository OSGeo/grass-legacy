#include "G3d.h"
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_getCoordsMap (map, rows, cols, depths)

     G3D_Map *map;
     int *rows, *cols, *depths;

{
  *rows = map->region.rows;
  *cols = map->region.cols;
  *depths = map->region.depths;
}

/*---------------------------------------------------------------------------*/

void
G3d_getCoordsMapWindow (map, rows, cols, depths)

     G3D_Map *map;
     int *rows, *cols, *depths;

{
  *rows = map->window.rows;
  *cols = map->window.cols;
  *depths = map->window.depths;
}

/*---------------------------------------------------------------------------*/

void
G3d_getNofTilesMap (map, nx, ny, nz)

     G3D_Map *map;
     int *nx, *ny, *nz;

{
  *nx = map->nx;
  *ny = map->ny;
  *nz = map->nz;
}

/*---------------------------------------------------------------------------*/

void
G3d_getRegionMap (map, north, south, east, west, top, bottom)

     G3D_Map *map;
     double *north, *south, *east, *west, *top, *bottom;

{
  *north = map->region.north;  
  *south = map->region.south;  
  *east = map->region.east;   
  *west = map->region.west;   
  *top = map->region.top;    
  *bottom = map->region.bottom; 
}

/*---------------------------------------------------------------------------*/

void
G3d_getWindowMap (map, north, south, east, west, top, bottom)

     G3D_Map *map;
     double *north, *south, *east, *west, *top, *bottom;

{
  *north = map->window.north;  
  *south = map->window.south;  
  *east = map->window.east;   
  *west = map->window.west;   
  *top = map->window.top;    
  *bottom = map->window.bottom; 
}

/*---------------------------------------------------------------------------*/

void
G3d_getRegionStructMap (map, region)

     G3D_Map *map;
     G3D_Region *region;

{
  G3d_regionCopy (region, &(map->region));
}

/*---------------------------------------------------------------------------*/

void
G3d_getWindowStructMap (map, window)

     G3D_Map *map;
     G3D_Region *window;

{
  G3d_regionCopy (window, &(map->window));
}

/*---------------------------------------------------------------------------*/

void
G3d_getTileDimensionsMap (map, x, y, z)

     G3D_Map *map;
     int *x, *y, *z;

{
  *x = map->tileX; 
  *y = map->tileY;
  *z = map->tileZ;
}

/*---------------------------------------------------------------------------*/

int
G3d_tileTypeMap (map)

     G3D_Map *map;

{
  return map->typeIntern;
}

/*---------------------------------------------------------------------------*/

int
G3d_fileTypeMap (map)

     G3D_Map *map;

{
  return map->type;
}

/*---------------------------------------------------------------------------*/

int
G3d_tilePrecisionMap (map)

     G3D_Map *map;

{
  return map->precision;
}

/*---------------------------------------------------------------------------*/

int
G3d_tileUseCacheMap (map)

     G3D_Map *map;

{
  return map->useCache;
}


void 
G3d_printHeader (map)

     G3D_Map *map;

{
  double rangeMin, rangeMax;

  printf ("File %s open for %sing:\n", map->fileName,
	  (map->operation == G3D_WRITE_DATA ? "writ" : 
	   (map->operation == G3D_READ_DATA ? "read" : "unknown")));
  printf ("  Fd = %d, Unit %s, Type: %s, ", map->data_fd,
	  map->unit,
	  (map->type == G3D_FLOAT ? "float" :
	   (map->type == G3D_DOUBLE ? "double" : "unknown")));
  printf ("Type intern: %s\n",
	  (map->typeIntern == G3D_FLOAT ? "float" :
	   (map->typeIntern == G3D_DOUBLE ? "double" : "unknown")));
  if (map->compression == G3D_NO_COMPRESSION)
    printf ("  Compression: none\n");
  else {
    printf ("  Compression:%s%s Precision: %s",
	    (map->useLzw ? " lzw," : ""), (map->useRle ? " rle," : ""),
	    (map->precision == -1 ? "all bits used\n" : "using"));
    if (map->precision != -1) printf (" %d bits\n", map->precision);
  }

  if (! map->useCache)
    printf ("  Cache: none\n");
  else {
    printf ("  Cache: used%s\n", 
	    (map->operation == G3D_WRITE_DATA ? ", File Cache used" : ""));
  }  

  G3d_range_min_max (map, &rangeMin, &rangeMax);
  
  printf ("  Region: (%f %f) (%f %f) (%f %f)\n", 
	  map->region.south, map->region.north, map->region.west, 
	  map->region.east, map->region.bottom, map->region.top);
  printf ("          (%d %d %d)\n", map->region.rows, map->region.cols, 
	  map->region.depths);
  printf ("  Tile size (%d %d %d)\n", map->tileX, map->tileY, map->tileZ);
  printf ("  Range (");
  if (G3d_isNullValueNum (&rangeMin, G3D_DOUBLE))
    printf ("NULL, "); 
  else
    printf ("%f, ", (double) rangeMin);
  if (G3d_isNullValueNum (&rangeMax, G3D_DOUBLE))
    printf ("NULL)\n"); 
  else
    printf ("%f)\n", (double) rangeMax);
  fflush (stdout);
}
     
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
