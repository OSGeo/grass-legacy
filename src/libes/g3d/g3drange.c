
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "gis.h"
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
      for (y = 0; y < rows; y++) {
	G_row_update_fp_range (tile, cols, range, cellType);
	tile += map->tileX * G3d_length (type);
      }
      if (yRedundant) tile += map->tileX * yRedundant * G3d_length (type);
    }
    return;
  }

  if (yRedundant) {
    for (z = 0; z < depths; z++) {
      G_row_update_fp_range (tile, map->tileX * rows, range, cellType);
      tile += map->tileXY * G3d_length (type);
    }
    return;
  }

  G_row_update_fp_range (tile, map->tileXY * depths, range, cellType);
}

/*---------------------------------------------------------------------------*/

int
G3d_readRange (name, mapset, drange) /* adapted from G_read_fp_range */

     char *name, *mapset;
     struct FPRange *drange;

{
  struct Range range;
  int fd;
  char buf[200], xdr_buf[100], buf2[200], xname[512], xmapset[512];
  DCELL dcell1, dcell2;
  XDR xdr_str;

  G_init_fp_range(drange);

  fd = -1;

  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, xname);
    sprintf (buf2, "%s@%s", G3D_RANGE_ELEMENT, xmapset); /* == range@mapset */
  } else {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, name);
    sprintf (buf2, "%s", G3D_RANGE_ELEMENT);
  }

  if (G_find_file2 (buf, buf2, mapset)) {
    fd = G_open_old (buf, buf2, mapset);
    if (fd < 0 ) goto error;

    if(read (fd, xdr_buf, 2 * G3D_XDR_DOUBLE_LENGTH) != 2 * G3D_XDR_DOUBLE_LENGTH )
      return 2;

    xdrmem_create (&xdr_str, xdr_buf, (u_int) G3D_XDR_DOUBLE_LENGTH * 2,
		   XDR_DECODE);
        
    /* if the f_range file exists, but empty */
    if (! xdr_double (&xdr_str, &dcell1) || ! xdr_double (&xdr_str, &dcell2)) 
      goto error;

    G_update_fp_range (dcell1, drange);
    G_update_fp_range (dcell2, drange);
    close(fd) ;
    return 1;
  }

error:
  if (fd > 0) close(fd);
  sprintf (buf, "can't read range file for [%s in %s]", name, mapset);
  G_warning (buf);
  return -1;
}

/*---------------------------------------------------------------------------*/

int
G3d_range_load (map) 

     G3D_Map *map;

{
  if (map->operation == G3D_WRITE_DATA) return 1;
  if (G3d_readRange (map->fileName, map->mapset, &(map->range)) == -1) {
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

/*-------------------------------------------------------------------------*/

static int
writeRange (name, range) /* adapted from G_write_fp_range */

     char *name;
     struct FPRange *range;

{
  int fd;
  char buf[200], xdr_buf[100], buf2[200], xname[512], xmapset[512];
  XDR xdr_str;

  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, xname);
    sprintf (buf2, "%s@%s", G3D_RANGE_ELEMENT, xmapset); /* == range@mapset */
  } else {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, name);
    sprintf (buf2, "%s", G3D_RANGE_ELEMENT);
  }

  fd = G_open_new (buf, buf2);
  if (fd < 0) goto error;

  if (range->first_time) {
    /* if range hasn't been updated, write empty file meaning NULLs */
    close (fd);
    return 0;
  }

  xdrmem_create (&xdr_str, xdr_buf, (u_int) G3D_XDR_DOUBLE_LENGTH * 2,
		 XDR_ENCODE);

  if (! xdr_double (&xdr_str, &(range->min))) goto error;
  if (! xdr_double (&xdr_str, &(range->max))) goto error;

  write (fd, xdr_buf, G3D_XDR_DOUBLE_LENGTH * 2);
  close (fd);
  return 0;

error:
  G_remove(buf, buf2); /* remove the old file with this name */
  sprintf (buf, "can't write range file for [%s in %s]", name, G_mapset());
  G_warning (buf);
  return -1;
}

/*---------------------------------------------------------------------------*/

int
G3d_range_write (map)

     G3D_Map *map;

{
  char path[4096], element[100];

  G3d_filename (path, G3D_RANGE_ELEMENT, map->fileName, map->mapset);
  unlink(path);

  if (writeRange (map->fileName, &(map->range)) == -1) {
    G3d_error ("G3d_closeCellNew: error in writeRange");
    return 0;
  }

  return 1;
}

/*---------------------------------------------------------------------------*/

int
G3d_range_init (map)

     G3D_Map *map;

{
  return G_init_fp_range (&(map->range));
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
