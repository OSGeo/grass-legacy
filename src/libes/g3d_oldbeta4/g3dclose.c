
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

static int
G3d_closeNew (map)

     G3D_Map *map;

{
  char path[4096], element[100], command[4096], buf[4096];
  int cell_fd;
  struct Categories cats;

  /* remove fcell/name file */
  G__file_name (path, "fcell", map->fileName, map->mapset);
  unlink (path);	

  /* remove cell_misc/name/f_format */
  sprintf(element, "cell_misc/%s", map->fileName);
  G__file_name (path, element, "f_format", map->mapset);
  unlink(path);

  /* remove cell_misc/name/f_quant */
  sprintf(element, "cell_misc/%s", map->fileName);
  G__file_name (path, element, "f_quant", map->mapset);
  unlink(path);

  /* create empty cell file */
  G__make_mapset_element ("cell");
  cell_fd = creat (G__file_name(path, "cell", 
				map->fileName, map->mapset), 0666);
  close(cell_fd);

  /* remove color table */
  G_remove_colr (map->fileName);

  /* create empty cats file */
  G_init_raster_cats ((char *) NULL, &cats);
  G_write_cats (map->fileName, &cats);
  G_free_cats (&cats);

  G3d_range_write (map);
  
  /* remove histogram */
  G_remove_histogram(map->fileName);

  close (map->data_fd);

  /* finally move tempfile to data file */
  G__file_name (path, G3D_CELL_DIR, map->fileName, map->mapset);
  if (link (map->tempName, path) < 0) {
    sprintf(command, "mv %s %s", map->tempName, path);
    if(system (command)) {
      sprintf (buf,
	   "G3d_closeNew: can't move temp raster file %s\nto 3d data file %s",
	       map->tempName, path);
      G3d_error (buf);
      return 0;
    }
  } else 
    unlink (map->tempName); 

  return 1;
}

/*---------------------------------------------------------------------------*/

static int
G3d_closeCellNew (map)

     G3D_Map *map;

{
  long ltmp;

  if (map->useCache)
    if (! G3d_flushAllTiles (map))  {
      G3d_error ("G3d_closeCellNew: error in G3d_flushAllTiles");
      return 0;
    }

  if (! G3d_flushIndex (map)) {
    G3d_error ("G3d_closeCellNew: error in G3d_flushIndex");
    return 0;
  }

  /* write the header info which was filled with dummy values at the */
  /* opening time */

  if (lseek(map->data_fd, 
	    (long) (map->offset - sizeof (int) - sizeof (long)), 
	    SEEK_SET) == -1) {
    G3d_error ("G3d_closeCellNew: can't position file");
    return 0;
  }

  if (! G3d_writeInts (map->data_fd, map->useXdr, 
		       &(map->indexNbytesUsed), 1)) {
    G3d_error ("G3d_closeCellNew: can't write header");
    return 0;
  }
    
  G3d_longEncode (&(map->indexOffset), &ltmp, 1);
  if (write (map->data_fd, &ltmp, sizeof (long)) != sizeof (long)) {
    G3d_error ("G3d_closeCellNew: can't write header");
    return 0;
  }

  if (! G3d_closeNew (map) != 0) {
    G3d_error ("G3d_closeCellNew: error in G3d_closeNew"); 
    return 0;
  }

  return 1;
}

/*---------------------------------------------------------------------------*/

static int
G3d_closeOld (map)

     G3D_Map *map;

{
  if (close (map->data_fd) != 0) {
    G3d_error ("G3d_closeOld: could not close file"); 
    return 0;
  }
  
  return 1;
}

/*---------------------------------------------------------------------------*/

static int
G3d_closeCellOld (map)

     G3D_Map *map;

{
  if (! G3d_closeOld (map) != 0) {
    G3d_error ("G3d_closeCellOld: error in G3d_closeOld"); 
    return 0;
  }

  return 1;
}
 
/*---------------------------------------------------------------------------*/

int
G3d_closeCell (map)

     G3D_Map *map;

{
  if (map->operation == G3D_WRITE_DATA) {
    if (! G3d_closeCellNew (map)) {
      G3d_error ("G3d_closeCell: error in G3d_closeCellNew");
      return 0;
    }
  } else {
    if (! G3d_closeCellOld (map) != 0) {
      G3d_error ("G3d_closeCell: error in G3d_closeCellOld"); 
      return 0;
    }
  }    

  G3d_free (map->index);
  G3d_free (map->tileLength);

  if (map->useCache) {
    if (! G3d_disposeCache (map)) {
      G3d_error ("G3d_closeCell: error in G3d_disposeCache"); 
      return 0;
    }
  } else 
    G3d_free (map->data);

  if (! G3d_writeHeader (map, 
			 map->region.north, map->region.south, 
			 map->region.east, map->region.west, 
			 map->region.top, map->region.bottom,
			 map->region.rows, map->region.cols, 
			 map->region.depths, 
			 map->tileX, map->tileY, map->tileZ,
			 map->type, 
			 map->compression, map->useRle, map->useLzw, 
			 map->precision, map->offset, map->useXdr,
			 map->hasIndex,
			 map->unit)) {
    G3d_error ("G3d_closeCell: error in G3d_writeHeader"); 
    return 0;
  }

  G3d_free (map->unit);
  G3d_free (map);
  return 1;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
