#include "gis.h"
#include "G3d.h"
#include <stdio.h>
#include "Viz.h"

/*---------------------------------------------------------------------------*/

void *map = NULL;

/*---------------------------------------------------------------------------*/
static void
fatalError (errorMsg)

     char *errorMsg;

{
  if (map != NULL) {
    /* should unopen map here! */
  }

  G3d_fatalError (errorMsg);
}

/*---------------------------------------------------------------------------*/

typedef	struct {
  struct Option *input, *output, *nv;
} paramType;

static paramType param;

static void
setParams ()

{
  param.input = G_define_option();
  param.input->key = "input";
  param.input->type = TYPE_STRING;
  param.input->required = YES;
  param.input->description = "Grid3 raster file to be imported";

  param.output = G_define_option();
  param.output->key = "output";
  param.output->type = TYPE_STRING;
  param.output->required = YES;
  param.output->multiple = NO ;
  param.output->gisprompt = "new,cell,raster(2d+3d)" ;
  param.output->description = "Name for G3d raster map";

  param.nv = G_define_option();
  param.nv->key = "nv";
  param.nv->type = TYPE_STRING;
  param.nv->required = NO;
  param.nv->multiple = NO;
  param.nv->answer = "no";
  param.nv->description = 
    "Indicates whether zeros should be converted to NULL values";
  param.nv->options = "yes,no";
}

/*---------------------------------------------------------------------------*/

static void
getParams (input, output, convertNull)

     char **input, **output;
     int *convertNull;

{
 *input = param.input->answer;
 *output = param.output->answer; 
 *convertNull = (strcmp(param.nv->answer, "yes") == 0);
}

/*---------------------------------------------------------------------------*/

static FILE *
openGrid3 (grid3File, grid3Header, region)

     char *grid3File;
     file_info *grid3Header;
     G3D_Region *region;

{
  grid3Header->datainfp = fopen (grid3File, "r");
  if (grid3Header->datainfp == NULL) {
    perror(grid3File);
    G_usage ();
    exit (-1) ;
  }

  g3read_header (grid3Header);
  G3d_getWindow (region);

  region->north = grid3Header->north;
  region->south = grid3Header->south;
  region->east = grid3Header->east;
  region->west = grid3Header->west;
  region->top = grid3Header->top;
  region->bottom = grid3Header->bottom;
  region->rows = grid3Header->xdim;
  region->cols = grid3Header->ydim;
  region->depths = grid3Header->zdim;

  return grid3Header->datainfp;
}

/*---------------------------------------------------------------------------*/

#define MAX(a,b) (a > b ? a : b)

static void
grid3ToG3d (grid3Header, convertNull)

     file_info *grid3Header;
     int convertNull;

{
  int x, y, z;
  float *slice;
  int tileX, tileY, tileZ;

  G3d_getTileDimensionsMap (map, &tileX, &tileY, &tileZ);
   G3d_minUnlocked (map, G3D_USE_CACHE_X);

  slice = G3d_malloc (sizeof (float) * grid3Header->ydim * grid3Header->xdim);

  if (slice == NULL)
    G3d_fatalError ("grid3ToG3d: can't allocate data for slice");

  G3d_autolockOn (map);
  G3d_unlockAll (map);

  for (z = 0; z < grid3Header->zdim; z++) {
    if ((z % tileZ) == 0) G3d_unlockAll (map);

    g3read_level (grid3Header, slice, z);
    if (convertNull) 
      for (y = 0; y < grid3Header->ydim; y++)
	for (x = 0; x < grid3Header->xdim; x++)
	  if (slice[y * grid3Header->xdim + x] == 0) 
	    G3d_setNullValue (slice + y * grid3Header->xdim + x, 1, G3D_FLOAT);

    for (y = 0; y < grid3Header->ydim; y++)
      for (x = 0; x < grid3Header->xdim; x++) 
	G3d_putFloat (map, x, y, z, slice[y * grid3Header->xdim + x]);

    if (! G3d_flushTilesInCube (map, 
				0, 0, MAX (0, z - tileZ),
				grid3Header->xdim - 1,
				grid3Header->ydim - 1, z))
      G3d_fatalError ("grid3Tog3d: error flushing tiles");
  }

  if (! G3d_flushAllTiles (map))  
    G3d_fatalError ("grid3Tog3d: error flushing tiles");

  G3d_autolockOff (map);
  G3d_unlockAll (map);
  G3d_free (slice);
}

/*---------------------------------------------------------------------------*/

main (argc, argv) 
     
     int argc;
     char *argv[];

{
  char *input, *output;
  int convertNull; 
  int useTypeDefault, type, useLzwDefault, doLzw, useRleDefault, doRle;
  int usePrecisionDefault, precision, useDimensionDefault, tileX, tileY, tileZ;
  file_info grid3Header;
  G3D_Region region;
  FILE *fp;

  map = NULL;

  G_gisinit(argv[0]);

  setParams ();
  G3d_setStandart3dInputParams ();

  if (G_parser(argc,argv)) exit(1);

  getParams (&input, &output, &convertNull);
  if (! G3d_getStandart3dParams (&useTypeDefault, &type, 
				 &useLzwDefault, &doLzw, 
				 &useRleDefault, &doRle, 
				 &usePrecisionDefault, &precision, 
				 &useDimensionDefault, &tileX, &tileY, &tileZ))
    fatalError ("main: error getting standard parameters");
  if (useTypeDefault) type = G3D_FLOAT; /* since grid3 file is in float */

  fp = openGrid3 (input, &grid3Header, &region);
  
  map = G3d_openNewParam (output, G3D_FLOAT, G3D_USE_CACHE_XY,
			  &region,
			  type, doLzw, doRle, precision, tileX, tileY, tileZ);

  if (map == NULL) G3d_fatalError ("main: error opening g3d file");

  grid3ToG3d (&grid3Header, convertNull);

  if (! G3d_closeCell (map)) 
    G3d_fatalError ("main: error closing new g3d file");
  map = NULL;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
