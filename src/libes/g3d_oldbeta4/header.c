/* corrected line 217 and line 220 - 10/99 M. Neteler*/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

char *tmpCompress = NULL;
int tmpCompressLength;
char *xdr = NULL;
int xdrLength;

/*---------------------------------------------------------------------------*/

#define G3D_HEADER_NORTH "North"
#define G3D_HEADER_SOUTH "South"
#define G3D_HEADER_EAST "East"
#define G3D_HEADER_WEST "West"
#define G3D_HEADER_TOP "Top"
#define G3D_HEADER_BOTTOM "Bottom"
#define G3D_HEADER_ROWS "nofRows"
#define G3D_HEADER_COLS "nofCols"
#define G3D_HEADER_DEPTHS "nofDepths"
#define G3D_HEADER_TILEX "TileDimensionX"
#define G3D_HEADER_TILEY "TileDimensionY"
#define G3D_HEADER_TILEZ "TileDimensionZ"
#define G3D_HEADER_TYPE "CellType"
#define G3D_HEADER_COMPRESSION "useCompression"
#define G3D_HEADER_USERLE "useRle"
#define G3D_HEADER_USELZW "useLzw"
#define G3D_HEADER_PRECISION "Precision"
#define G3D_HEADER_DATA_OFFSET "nofHeaderBytes"
#define G3D_HEADER_USEXDR "useXdr"
#define G3D_HEADER_HASINDEX "hasIndex"
#define G3D_HEADER_UNIT "Units"

#define G3D_HEADER_ELEMENT "cellhd"

/*---------------------------------------------------------------------------*/

static int
G3d_getHeaderInt (headerKeys, key, i)

     struct Key_Value *headerKeys;
     char *key;
     int *i;

{
  char msg[1024];
  char *str;

  if ((str = G_find_key_value (key, headerKeys)) == NULL) {
    sprintf (msg, 
	     "G3d_getHeaderInt: cannot find field %s in header", key);
    G3d_error (msg);
    return 0;
  }
  
  G_strip(str);
  if (sscanf (str, "%d", i) == 1) return 1;

  sprintf (msg, "G3d_getHeaderInt: invalid value: field %s in header", key);
  G3d_error (msg);
  return 0;
}

/*---------------------------------------------------------------------------*/

static int
G3d_getHeaderDouble (headerKeys, key, d)

     struct Key_Value *headerKeys;
     char *key;
     double *d;

{
  char msg[1024];
  char *str;

  if ((str = G_find_key_value (key, headerKeys)) == NULL) {
    sprintf (msg, 
	     "G3d_getHeaderDouble: cannot find field %s in header", key);
    G3d_error (msg);
    return 0;
  }
  
  G_strip(str);
  if (sscanf (str, "%lf", d) == 1) return 1;

  sprintf (msg, 
	   "G3d_getHeaderDouble: invalid value: field %s in header", key);
  G3d_error (msg);
  return 0;
}

/*---------------------------------------------------------------------------*/

static int
G3d_getHeaderString (headerKeys, key, returnStr)

     struct Key_Value *headerKeys;
     char *key, **returnStr;

{
  char msg[1024];
  char *str;

  if ((str = G_find_key_value (key, headerKeys)) == NULL) {
    sprintf (msg, 
	     "G3d_getHeaderString: cannot find field %s in header", key);
    G3d_error (msg);
    return 0;
  }

  G_strip(str);
  *returnStr = G_store (str);
  return 1;
}

/*---------------------------------------------------------------------------*/

static int
G3d_getHeaderValue (headerKeys, key, val1, val2, result1, result2, resultVar)

     struct Key_Value *headerKeys;
     char *key, *val1, *val2;
     int result1, result2;
     int *resultVar;

{
  char msg[1024];
  char *str;

  if ((str = G_find_key_value (key, headerKeys)) == NULL) {
    sprintf (msg, 
	     "G3d_getHeaderValue: cannot find field %s in header", key);
    G3d_error (msg);
    return 0;
  }

  G_strip(str);
  if (strcmp (str, val1) == 0) {
    *resultVar = result1;
    return 1;
  }
  if (strcmp (str, val2) == 0) {
    *resultVar = result2;
    return 1;
  }

  sprintf (msg, "G3d_getHeaderValue: invalid type: field %s in header", key);
  G3d_error (msg);
  return 0;
}

/*---------------------------------------------------------------------------*/

static int
G3d_setHeaderInt (headerKeys, key, i)

     struct Key_Value *headerKeys;
     char *key;
     int *i;

{
  char keyValStr[200];

  sprintf (keyValStr, "%d", *i);
  return (G_set_key_value (key, keyValStr, headerKeys) != 0);
}

/*---------------------------------------------------------------------------*/

static int
G3d_setHeaderDouble (headerKeys, key, d)

     struct Key_Value *headerKeys;
     char *key;
     double *d;

{
  char keyValStr[200];

  sprintf (keyValStr, "%.50lf", *d); 
  return (G_set_key_value (key, keyValStr, headerKeys) != 0);
}

/*---------------------------------------------------------------------------*/

static int
G3d_setHeaderString (headerKeys, key, keyValStr)

     struct Key_Value *headerKeys;
     char *key, **keyValStr;

{
  return (G_set_key_value (key, *keyValStr, headerKeys) != 0);
}

/*---------------------------------------------------------------------------*/

static int
G3d_setHeaderValue (headerKeys, key, val1, val2, keyval1, keyval2, keyvalVar)

     struct Key_Value *headerKeys;
     char *key, *val1, *val2;
     int keyval1, keyval2;
     int *keyvalVar;

{
/* corrected 10/99 M. Neteler
  if (*keyvalVar == keyval1) 
/*    return (G_set_key_value (key, val1, headerKeys, headerKeys) != 0); */
       return (G_set_key_value (key, val1, headerKeys) != 0);
  if (*keyvalVar == keyval2) 
/*    return (G_set_key_value (key, val2, headerKeys, headerKeys) != 0); */
       return (G_set_key_value (key, val2, headerKeys) != 0);
       
  G3d_error ("G3d_setHeaderValue: wrong key value");
  return 0;
}

/*---------------------------------------------------------------------------*/

static int
G3d_readWriteHeader (headerKeys, doRead,
		     north, south, east, west, top, bottom,
		     rows, cols, depths,
		     tileX, tileY, tileZ,
		     type, 
		     compression, useRle, useLzw, precision,
		     dataOffset, useXdr, hasIndex, unit)

     struct Key_Value *headerKeys;
     int doRead;
     double *north, *south, *east, *west, *top, *bottom;
     int *rows, *cols, *depths, *tileX, *tileY, *tileZ, *type, *compression;
     int *useRle, *useLzw, *precision, *dataOffset, *useXdr, *hasIndex;
     char **unit;

{
  char msg[1024];
  int returnVal;
  int (*headerInt) (), (*headerDouble) (), (*headerValue) ();
  int (*headerString) ();

  if (doRead) {
    headerDouble = G3d_getHeaderDouble;
    headerInt = G3d_getHeaderInt;
    headerString = G3d_getHeaderString;
    headerValue = G3d_getHeaderValue;
  } else {
    headerDouble = G3d_setHeaderDouble;
    headerInt = G3d_setHeaderInt;
    headerString = G3d_setHeaderString;
    headerValue = G3d_setHeaderValue;
  }

  returnVal = 1;
  returnVal &= headerDouble (headerKeys, G3D_HEADER_NORTH, north);
  returnVal &= headerDouble (headerKeys, G3D_HEADER_SOUTH, south);
  returnVal &= headerDouble (headerKeys, G3D_HEADER_EAST, east );
  returnVal &= headerDouble (headerKeys, G3D_HEADER_WEST, west);
  returnVal &= headerDouble (headerKeys, G3D_HEADER_TOP, top);
  returnVal &= headerDouble (headerKeys, G3D_HEADER_BOTTOM, bottom);

  returnVal &= headerInt (headerKeys, G3D_HEADER_ROWS, rows);
  returnVal &= headerInt (headerKeys, G3D_HEADER_COLS, cols);
  returnVal &= headerInt (headerKeys, G3D_HEADER_DEPTHS, depths);

  returnVal &= headerInt (headerKeys, G3D_HEADER_TILEX, tileX);
  returnVal &= headerInt (headerKeys, G3D_HEADER_TILEY, tileY);
  returnVal &= headerInt (headerKeys, G3D_HEADER_TILEZ, tileZ);

  returnVal &= headerValue (headerKeys, G3D_HEADER_TYPE, 
			    "double", "float", G3D_DOUBLE, G3D_FLOAT, type);
  returnVal &= headerValue (headerKeys, G3D_HEADER_COMPRESSION, 
			    "0", "1", 0, 1, compression);
  returnVal &= headerValue (headerKeys, G3D_HEADER_USERLE, 
			    "0", "1", 0, 1, useRle);
  returnVal &= headerValue (headerKeys, G3D_HEADER_USELZW, 
			    "0", "1", 0, 1, useLzw);

  returnVal &= headerInt (headerKeys, G3D_HEADER_PRECISION, precision);
  returnVal &= headerInt (headerKeys, G3D_HEADER_DATA_OFFSET, dataOffset);

  returnVal &= headerValue (headerKeys, G3D_HEADER_USEXDR, 
			    "0", "1", 0, 1, useXdr);
  returnVal &= headerValue (headerKeys, G3D_HEADER_HASINDEX, 
			    "0", "1", 0, 1, hasIndex);
  returnVal &= headerString (headerKeys, G3D_HEADER_UNIT, unit);

  if (returnVal) return 1;

  G3d_error ("G3d_readWriteHeader: error reading header");
  return 0;
}

/*---------------------------------------------------------------------------*/

int
G3d_readHeader (map,
		north, south, east, west, top, bottom,
		rows, cols, depths,
		tileX, tileY, tileZ,
		type, 
		compression, useRle, useLzw, precision,
		dataOffset, useXdr, hasIndex, unit)

     G3D_Map *map;
     double *north, *south, *east, *west, *top, *bottom;
     int *rows, *cols, *depths, *tileX, *tileY, *tileZ, *type, *compression;
     int *useRle, *useLzw, *precision, *dataOffset, *useXdr, *hasIndex;
     char **unit;

{
  struct Key_Value *headerKeys;
  char path[1024], msg[1024];
  int status, returnVal;

  G__file_name(path, G3D_HEADER_ELEMENT, map->fileName, map->mapset);
  if (access(path, R_OK) != 0) {
    sprintf (msg,"G3d_readHeader: unable to find [%s]", path);
    G3d_error (msg);
    return 0;
  }
  
  headerKeys = G_read_key_value_file (path, &status);
  if (status != 0) {
    sprintf (msg, "G3d_readHeader: Unable to open %s", path);
    G3d_error (msg);
    return 0;
  }

  if (! G3d_readWriteHeader (headerKeys, 1, 
			     north, south, east, west, top, bottom,
			     rows, cols, depths, tileX, tileY, tileZ,
			     type, compression, useRle, useLzw, precision,
			     dataOffset, useXdr, hasIndex, unit)) {
    sprintf (msg, "G3d_readHeader: error extracting header key(s) of file %s",
	     path);
    G3d_error (msg);
    return 0;
  }

  G_free_key_value(headerKeys);
  return 1;
}

/*---------------------------------------------------------------------------*/

int
G3d_writeHeader (map,
		 north, south, east, west, top, bottom,
		 rows, cols, depths,
		 tileX, tileY, tileZ,
		 type, 
		 compression, useRle, useLzw, precision,
		 dataOffset, useXdr, hasIndex, unit)

     G3D_Map *map;
     double north, south, east, west, top, bottom;
     int rows, cols, depths, tileX, tileY, tileZ, type, compression;
     int useRle, useLzw, precision, dataOffset, useXdr, hasIndex;
     char *unit;

{
  struct Key_Value *headerKeys;
  char path[1024], msg[1024];
  int status;

  headerKeys = G_create_key_value();

  if (! G3d_readWriteHeader (headerKeys, 0, 
			     &north, &south, &east, &west, &top, &bottom,
			     &rows, &cols, &depths, &tileX, &tileY, &tileZ,
			     &type, &compression, &useRle, &useLzw, 
			     &precision, &dataOffset, &useXdr, &hasIndex,
			     &unit)) {
    sprintf (msg, "G3d_writeHeader: error adding header key(s) for file %s", 
	     path);
    G3d_error (msg);
    return 0;
  }

  G__file_name(path, G3D_HEADER_ELEMENT, map->fileName, map->mapset);
  G__make_mapset_element (G3D_HEADER_ELEMENT);
  G_write_key_value_file (path, headerKeys, &status);

  G_free_key_value(headerKeys);

  if (status == 0) return 1;

  sprintf (msg, "G3d_writeHeader: error writing header file %s", path);
  G3d_error (msg);
  return 0;
}
  
/*---------------------------------------------------------------------------*/

/* this function does actually more than filling the header fields of the */
/* G3D-Map structure. It also allocates memory for compression and xdr, */
/* and initializes the index and cache. This function should be taken apart. */

int
G3d_fillHeader (map, operation, compression, useRle, useLzw, type, precision, 
		cache, hasIndex, useXdr, 
		typeIntern, nofHeaderBytes, 
		tileX, tileY, tileZ, 
		north, south, east, west, top, bottom, rows, cols, depths,
		unit)

     G3D_Map *map;
     int compression, useRle, useLzw, operation, precision, cache, hasIndex;
     int useXdr, type, nofHeaderBytes, tileX, tileY, tileZ;
     double north, south, east, west, top, bottom;
     int rows, cols, depths, typeIntern;
     char *unit;

{
  if (! G3D_VALID_OPERATION (operation)) 
    G3d_fatalError ("G3d_fillHeader: operation not valid\n");

  map->operation = operation;

  map->unit = G_store (unit);
  
  map->region.north = north;
  map->region.south = south;
  map->region.east = east;
  map->region.west = west;
  map->region.top = top;
  map->region.bottom = bottom;

  map->region.rows = rows; 
  map->region.cols = cols; 
  map->region.depths = depths;

  map->tileX = tileX;
  map->tileY = tileY;
  map->tileZ = tileZ;
  map->tileXY = map->tileX * map->tileY;
  map->tileSize = map->tileXY * map->tileZ;

  map->nx = (map->region.rows - 1) / tileX + 1;
  map->ny = (map->region.cols - 1) / tileY + 1;
  map->nz = (map->region.depths - 1) / tileZ + 1;
  map->nxy = map->nx * map->ny;
  map->nTiles = map->nxy * map->nz;
  
  if ((map->region.rows) % map->tileX != 0) map->clipX = map->nx - 1;
  else map->clipX = -1;
  if ((map->region.cols) % map->tileY != 0) map->clipY = map->ny - 1;
  else map->clipY = -1;
  if ((map->region.depths) % map->tileZ != 0) map->clipZ = map->nz - 1;
  else map->clipZ = -1;

  if ((type != G3D_FLOAT) && (type != G3D_DOUBLE))
    G3d_fatalError ("G3d_fillHeader: invalid type");
  map->type = type;

  if ((typeIntern != G3D_FLOAT) && (typeIntern != G3D_DOUBLE))
    G3d_fatalError ("G3d_fillHeader: invalid type");
  map->typeIntern = typeIntern;

  if (! G3D_VALID_XDR_OPTION (useXdr))
    G3d_fatalError ("G3d_fillHeader: invalid xdr option");
  map->useXdr = useXdr;

  map->offset = nofHeaderBytes;

  if ((map->fileEndPtr = lseek (map->data_fd, (long) 0, SEEK_END)) == -1) {
    G3d_error ("G3d_fillHeader: can't position file");
    return 0;
  }

  map->useCache = (cache != G3D_NO_CACHE);

  map->numLengthIntern = G3d_length (map->typeIntern);
  map->numLengthExtern = G3d_externLength (map->type);

  map->compression = compression;
  map->useRle = useRle;
  map->useLzw = useLzw;
  map->precision = precision;

#define RLE_STATUS_BYTES 2

  if (map->compression != G3D_NO_COMPRESSION) {
    if (tmpCompress == NULL) {
      tmpCompressLength = map->tileSize * 
                         G3D_MAX (map->numLengthIntern, map->numLengthExtern) +
			 RLE_STATUS_BYTES;
      tmpCompress = G3d_malloc (tmpCompressLength);
      if (tmpCompress == NULL) {
	G3d_error ("G3d_fillHeader: error in G3d_malloc");
	return 0;
      }
    } else
      if (map->tileSize * G3D_MAX (map->numLengthIntern, map->numLengthExtern)
	  + RLE_STATUS_BYTES > tmpCompressLength) {
	tmpCompressLength = map->tileSize * 
	                 G3D_MAX (map->numLengthIntern, map->numLengthExtern) +
			 RLE_STATUS_BYTES;
	tmpCompress = G3d_realloc (tmpCompress, tmpCompressLength);
	if (tmpCompress == NULL) {
	  G3d_error ("G3d_fillHeader: error in G3d_realloc");
	  return 0;
	}
      }
  }

#define XDR_MISUSE_BYTES 10

  if (! G3d_initFpXdr (map, XDR_MISUSE_BYTES)) {
    G3d_error ("G3d_fillHeader: error in G3d_initFpXdr");
    return 0;
  }

  if ((! map->useCache) ||
      ((cache == G3D_USE_CACHE_DEFAULT) && (g3d_cache_default == 0))) {
    map->useCache = 0;
    map->cache = NULL;
    /* allocate one tile buffer */
    map->data = G3d_malloc (map->tileSize * map->numLengthIntern);
    if (map->data == NULL) {
      G3d_error ("G3d_fillHeader: error in G3d_malloc");
      return 0;
    }
    map->currentIndex = -1;
  } else {
    if (cache == G3D_USE_CACHE_DEFAULT) {
      if (! G3d_initCache (map, 
			   G3D_MAX (1, 
			    G3D_MIN (g3d_cache_default, 
				     g3d_cache_max / 
				     map->tileSize / map->numLengthIntern)))) {
	G3d_error ("G3d_fillHeader: error in G3d_initCache");
	return 0;
      }
    } else
      if (! G3d_initCache (map, G3D_MAX (1, G3D_MIN (cache, 
				     g3d_cache_max / 
				     map->tileSize / map->numLengthIntern)))) {
	G3d_error ("G3d_fillHeader: error in G3d_initCache");
	return 0;
      }
  }

  if (! G3d_initIndex (map, hasIndex)) {
    G3d_error ("G3d_fillHeader: error in G3d_initIndex");
    return 0;
  }

  return 1;
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
