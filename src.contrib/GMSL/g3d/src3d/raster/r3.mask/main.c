
#include "gis.h"
#include "G3d.h"

/*--------------------------------------------------------------------------*/

typedef struct {
  struct Option *map, *maskVals;
} paramType;

static paramType params;
extern void * G3d_openNewParam ();

static void
setParams ()

{
  params.map = G_define_option();
  params.map->key = "grid3";
  params.map->type = TYPE_STRING ;
  params.map->required = YES ;
  params.map->multiple = NO ;
  params.map->gisprompt = "old,grid3,3d-raster";
  params.map->description = "3dcell map with reference values";

  params.maskVals = G_define_option();
  params.maskVals->key = "maskvalues";
  params.maskVals->key_desc = "val[-val]";
  params.maskVals->type = TYPE_STRING ;
  params.maskVals->required = NO ;
  params.maskVals->multiple = YES ;
  params.maskVals->description = "List of cell values to be masked out";
}

/*--------------------------------------------------------------------------*/

void
getParams (name, maskRules)

     char **name;
     void **maskRules;

{
 *name = params.map->answer;
 parse_vallist (params.maskVals->answers, maskRules);
}

/*-------------------------------------------------------------------------*/

#define MAX(a,b) (a > b ? a : b)

static void
makeMask (name, maskRules)

     char *name;
     void *maskRules;

{
  void *map, *mask;
  G3D_Region region;
  int tileX, tileY, tileZ, x, y, z, cacheSize;
  double value;
  float floatNull;

  cacheSize = G3d_cacheSizeEncode (G3D_USE_CACHE_XY, 1);

  map = G3d_openCellOld (name, G_mapset (), G3D_DEFAULT_WINDOW,
			 G3D_DOUBLE, cacheSize);
  if (map == NULL) G3d_fatalError ("makeMask: error opening map");

  G3d_getRegionStructMap (map, &region);
  G3d_getTileDimensionsMap (map, &tileX, &tileY, &tileZ);

  mask = G3d_openNewParam (G3d_maskFile (), G3D_FLOAT, cacheSize,
			   &region, G3D_FLOAT, G3D_NO_LZW, G3D_USE_RLE, 0,
			   tileX, tileY, tileZ);
  if (mask == NULL) G3d_fatalError ("makeMask: error opening g3d mask file");

  G3d_minUnlocked (map, G3D_USE_CACHE_X);
  G3d_autolockOn (map);
  G3d_unlockAll (map);
  G3d_minUnlocked (mask, G3D_USE_CACHE_X);
  G3d_autolockOn (mask);
  G3d_unlockAll (mask);

  G3d_setNullValue (&floatNull, 1, G3D_FLOAT);

  for (z = 0; z < region.depths; z++) {
    if ((z % tileZ) == 0) {
      G3d_unlockAll (map);
      G3d_unlockAll (mask);
    }
    for (y = 0; y < region.cols; y++)
      for (x = 0; x < region.rows; x++) {
	value = G3d_getDoubleRegion (map, x, y, z);
	if (mask_d_select ((DCELL *) &value, maskRules))
	  G3d_putFloat (mask, x, y, z, floatNull); /* mask-out value */
	else
	  G3d_putFloat (mask, x, y, z, (float) 0.); /* not mask-out value */
      }

    if (! G3d_flushTilesInCube (mask, 
				0, 0, MAX (0, z - tileZ),
				region.rows - 1,
				region.cols - 1, z))
      G3d_fatalError ("makeMask: error flushing tiles");
  }

  if (! G3d_flushAllTiles (mask))  
    G3d_fatalError ("makeMask: error flushing tiles");

  G3d_autolockOff (map);
  G3d_unlockAll (map);
  G3d_autolockOff (mask);
  G3d_unlockAll (mask);

  if (! G3d_closeCell (mask)) 
    G3d_fatalError ("makeMask: error closing g3d mask file");
  if (! G3d_closeCell (map)) 
    G3d_fatalError ("makeMask: error closing map");
}

/*--------------------------------------------------------------------------*/

main (argc, argv) 

     int argc;
     char *argv[];

{
  char *name;
  void *maskRules;

  G_gisinit (argv[0]);

  if (G3d_maskFileExists ()) {
    printf ("\n\n   Cannot create mask file: 3d-mask already exists!\n");
    printf 
      ("   Use 'g.remove g3d=G3D_MASK' or 'rm -r $LOCATION/grid3/G3D_MASK'\n");
    printf ("   to remove the existing mask.\n\n");
    exit (1);
  }

  setParams ();
  if (G_parser (argc, argv)) exit(1);
  getParams (&name, &maskRules);

  makeMask (name, maskRules);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
