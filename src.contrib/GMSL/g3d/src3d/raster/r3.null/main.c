
#include "gis.h"
#include "G3d.h"

/*--------------------------------------------------------------------------*/

typedef struct {
  struct Option *map, *setNull, *null;
} paramType;

static paramType params;
extern void * G3d_openNewParam ();

static void
setParams ()

{
  params.map = G_define_option();
  params.map->key = "map";
  params.map->type = TYPE_STRING ;
  params.map->required = YES ;
  params.map->multiple = NO ;
  params.map->gisprompt = "old,grid3,3d-raster";
  params.map->description = "G3d raster map for which to modify null values";

  params.setNull = G_define_option();
  params.setNull->key = "setnull";
  params.setNull->key_desc = "val[-val]";
  params.setNull->type = TYPE_STRING ;
  params.setNull->required = NO ;
  params.setNull->multiple = YES ;
  params.setNull->description = "List of cell values to be set to NULL";

  params.null = G_define_option();
  params.null->key = "null";
  params.null->type = TYPE_DOUBLE;
  params.null->required = NO;
  params.null->multiple = NO;
  params.null->description = "The value to replace the null value by";
}

/*--------------------------------------------------------------------------*/

void
getParams (name, maskRules, changeNull, newNullVal)

     char **name;
     void **maskRules;
     int *changeNull;
     double *newNullVal;

{
 *name = params.map->answer;
 parse_vallist (params.setNull->answers, maskRules);

 *changeNull = (params.null->answer != NULL);
 if (*changeNull)
   if (sscanf (params.null->answer, "%lf", newNullVal) != 1)
     G3d_fatalError ("illegal value for null");
}

/*-------------------------------------------------------------------------*/

#define MAX(a,b) (a > b ? a : b)

static void
modifyNull (name, maskRules, changeNull, newNullVal)

     char *name;
     void *maskRules;
     int changeNull;
     double newNullVal;

{
  void *map, *mapOut;
  G3D_Region region;
  int tileX, tileY, tileZ, x, y, z;
  double value;
  int doCompress, doLzw, doRle, precision;

  map = G3d_openCellOld (name, G_mapset (), G3D_DEFAULT_WINDOW,
			 G3D_DOUBLE, G3D_USE_CACHE_XY);
  if (map == NULL) G3d_fatalError ("modifyNull: error opening map");

  G3d_getRegionStructMap (map, &region);
  G3d_getTileDimensionsMap (map, &tileX, &tileY, &tileZ);

  G3d_getCompressionMode (&doCompress, &doLzw, &doRle, &precision);

  mapOut = G3d_openNewParam (name, G3D_DOUBLE, G3D_USE_CACHE_XY,
			     &region, G3d_fileTypeMap (map), 
			     doLzw, doRle, G3d_tilePrecisionMap (map),
			     tileX, tileY, tileZ);
  if (mapOut == NULL) 
    G3d_fatalError ("modifyNull: error opening tmp file");

  G3d_minUnlocked (map, G3D_USE_CACHE_X);
  G3d_autolockOn (map);
  G3d_unlockAll (map);
  G3d_minUnlocked (mapOut, G3D_USE_CACHE_X);
  G3d_autolockOn (mapOut);
  G3d_unlockAll (mapOut);

  for (z = 0; z < region.depths; z++) {
    if ((z % tileZ) == 0) {
      G3d_unlockAll (map);
      G3d_unlockAll (mapOut);
    }
    for (y = 0; y < region.cols; y++)
      for (x = 0; x < region.rows; x++) {

	value = G3d_getDoubleRegion (map, x, y, z);

	if (G3d_isNullValueNum (&value, G3D_DOUBLE)) {
	  if (modifyNull) {
	    value = newNullVal;
	  }
	} else
	  if (mask_d_select ((DCELL *) &value, maskRules)) {
	    G3d_setNullValue (&value, 1, G3D_DOUBLE);
	  }

	G3d_putDouble (mapOut, x, y, z, value);
      }

    if (! G3d_flushTilesInCube (mapOut, 0, 0, MAX (0, z - tileZ), 
				region.rows - 1, region.cols - 1, z))
      G3d_fatalError ("modifyNull: error flushing tiles");
  }

  if (! G3d_flushAllTiles (mapOut))  
    G3d_fatalError ("modifyNull: error flushing tiles");

  G3d_autolockOff (map);
  G3d_unlockAll (map);
  G3d_autolockOff (mapOut);
  G3d_unlockAll (mapOut);

  if (! G3d_closeCell (map)) 
    G3d_fatalError ("modifyNull: error closing map");
  if (! G3d_closeCell (mapOut)) 
    G3d_fatalError ("modifyNull: error closing tmp file");
}

/*--------------------------------------------------------------------------*/

main (argc, argv) 

     int argc;
     char *argv[];

{
  char *name;
  void *maskRules;
  int changeNull;
  double newNullVal;

  G_gisinit (argv[0]);

  setParams ();
  if (G_parser (argc, argv)) exit(1);
  getParams (&name, &maskRules, &changeNull, &newNullVal);

  modifyNull (name, maskRules, changeNull, newNullVal);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
