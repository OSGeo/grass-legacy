#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void *
G3d_openCellOldNoHeader (name, mapset)

     char *name, *mapset;

{
  G3D_Map *map;

  G3d_initDefaults ();

  map = G3d_malloc (sizeof (G3D_Map));
  if (map == NULL) {
    G3d_error ("G3d_openCellOldNoHeader: error in G3d_malloc");
    return (void *) NULL;
  }

  map->fileName = G_store (name);
  map->mapset = G_store (mapset);

  map->data_fd = G_open_old (G3D_CELL_DIR, name, mapset);
  if (map->data_fd < 0) {
    G3d_error ("G3d_openCellOldNoHeader: error in G_open_old");
    return (void *) NULL;
  } 

  G3d_range_init (map);

  return map;
}

/*---------------------------------------------------------------------------*/

void *
G3d_openCellOld (name, mapset, typeIntern, cache)

     char *name, *mapset;
     int typeIntern, cache;

{
  G3D_Map *map; 
  int compression, useRle, useLzw, type, tileX, tileY, tileZ;
  int rows, cols, depths, precision;
  int nofHeaderBytes, dataOffset, useXdr, hasIndex;
  char *ltmp, *unit;
  double north, south, east, west, top, bottom;

  map = G3d_openCellOldNoHeader (name, mapset);
  if (map == NULL) {
    G3d_error ("G3d_openCellOld: error in G3d_openCellOldNoHeader");
    return (void *) NULL;
  }

  if (lseek(map->data_fd, (long) 0, SEEK_SET) == -1) {
    G3d_error ("G3d_openCellOld: can't rewind file");
    return (void *) NULL;
  }

  if (! G3d_readHeader (map, 
			&north, &south, &east, &west, &top, &bottom,
			&rows, &cols, &depths, &tileX, &tileY, &tileZ,
			&type, &compression, &useRle, &useLzw, 
			&precision, &dataOffset, &useXdr, &hasIndex,
			&unit)) {
    G3d_error ("G3d_openCellOld: error in G3d_readHeader"); 
    return 0;
  }

  map->useXdr = useXdr;
  

  if (hasIndex) {
    /* see G3D_openCell_new () for format of header */
    if ((! G3d_readInts (map->data_fd, map->useXdr, 
			 &(map->indexLongNbytes), 1)) ||
	(! G3d_readInts (map->data_fd, map->useXdr, 
			 &(map->indexNbytesUsed), 1))) {
      G3d_error ("G3d_openCellOld: can't read header");
      return (void *) NULL;
    }

    /* if our long is to short to store offsets we can't read the file */
    if (map->indexNbytesUsed > sizeof (long))
      G3d_fatalError ("G3d_openCellOld: index does not fit into long");

    ltmp = G3d_malloc (map->indexNbytesUsed);
    if (ltmp == NULL) {
      G3d_error ("G3d_openCellOld: error in G3d_malloc");
      return (void *) NULL;
    }

    /* convert file long to long */
    if (read (map->data_fd, ltmp, map->indexLongNbytes) != 
	map->indexLongNbytes) {
      G3d_error ("G3d_openCellOld: can't read header");
      return (void *) NULL;
    }
    G3d_longDecode (ltmp, &(map->indexOffset), 1, map->indexLongNbytes);
    G3d_free (ltmp);
  }

  nofHeaderBytes = dataOffset;

  if (typeIntern == G3D_TILE_SAME_AS_FILE) typeIntern = type;

  if (! G3d_fillHeader (map, G3D_READ_DATA, compression, useRle, useLzw,
			type, precision, cache,
			hasIndex, map->useXdr, typeIntern, 
			nofHeaderBytes, tileX, tileY, tileZ, 
			north, south, east, west, top, bottom, 
			rows, cols, depths, unit)) {
    G3d_error ("G3d_openCellOld: error in G3d_fillHeader");
    return (void *) NULL;
  }

  return map;
}

/*---------------------------------------------------------------------------*/

void *
G3d_openCellNew (name, typeIntern, cache, region)

     char *name;
     int typeIntern, cache;
     G3D_Region *region;

{
  G3D_Map *map;
  int nofHeaderBytes, dummy = 0, compression, precision;
  long ldummy = 0;

  G3d_initDefaults ();

  compression = g3d_do_compression;
  precision = g3d_precision;

  map = G3d_malloc (sizeof (G3D_Map));
  if (map == NULL) {
    G3d_error ("G3d_openCellNew: error in G3d_malloc");
    return (void *) NULL;
  }

  map->fileName = G_store (name);
  map->mapset = G_store (G_mapset ());

  map->tempName = G_tempfile ();
  map->data_fd = open (map->tempName, O_RDWR | O_CREAT | O_TRUNC, 0666);
  if (map->data_fd < 0) {
    G3d_error ("G3d_openCellNew: could not open file");
    return (void *) NULL;
  } 

  G__make_mapset_element (G3D_CELL_DIR);

  map->useXdr = G3D_USE_XDR;

  if (g3d_file_type == G3D_FLOAT) {
    if (precision > 23) precision = 23; /* 32 - 8 - 1 */
    else
      if (precision < -1) precision = 0;
  } else
    if (precision > 52) precision = 52; /* 64 - 11 - 1 */
    else
      if (precision < -1) precision = 0;

  /* no need to write trailing zeros */
  if ((typeIntern == G3D_FLOAT) && (g3d_file_type == G3D_DOUBLE)) 
    if (precision == -1) 
      precision = 23;
    else
      precision = G3D_MIN (precision, 23);

  if (compression == G3D_NO_COMPRESSION) precision = G3D_MAX_PRECISION;
  if (compression == G3D_COMPRESSION) map->useXdr = G3D_USE_XDR;

  if (G3D_HAS_INDEX) {
    map->indexLongNbytes = sizeof (long);

    /* at the beginning of the file write */
    /*      nof bytes of "long" */
    /*      max nof bytes used for index */
    /*      position of index in file */
    /* the index is appended at the end of the file at closing time. since */
    /* we do not know this position yet we write dummy values */

    if ((! G3d_writeInts (map->data_fd, map->useXdr, 
			  &(map->indexLongNbytes), 1)) ||
	(! G3d_writeInts (map->data_fd, map->useXdr, &dummy, 1))) {
      G3d_error ("G3d_openCellNew: can't write header");
      return (void *) NULL;
    }
    if (write (map->data_fd, &ldummy, map->indexLongNbytes) != 
	map->indexLongNbytes) {
      G3d_error ("G3d_openCellNew: can't write header");
      return (void *) NULL;
    }
  }

  /* can't use a constant since this depends on sizeof (long) */
  nofHeaderBytes = lseek (map->data_fd, (long) 0, SEEK_CUR);

  G3d_range_init (map);

  if (! G3d_fillHeader (map, G3D_WRITE_DATA, compression, 
			g3d_do_rle_compression, g3d_do_lzw_compression, 
			g3d_file_type, precision, cache, G3D_HAS_INDEX, 
			map->useXdr, typeIntern, nofHeaderBytes, 
			g3d_tile_dimension[0], g3d_tile_dimension[1], 
			g3d_tile_dimension[2], 
			region->north, region->south, region->east, 
			region->west, region->top, region->bottom, 
			region->rows, region->cols, region->depths,
			g3d_unit_default)) {
    G3d_error ("G3d_openCellNew: error in G3d_fillHeader");
    return (void *) NULL;
  }

  return (void *) map;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
