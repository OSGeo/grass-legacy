#include "gis.h"
#include "G3d_intern.h"

/*----------------------------------------------------------------------------*/

typedef struct {

  struct Option *type;
  struct Option *precision;
  struct Option *compression;
  struct Option *dimension;
  struct Option *cache;

} G3d_paramType;

/*----------------------------------------------------------------------------*/

static G3d_paramType *param;

void
G3d_setStandart3dInputParams ()

{
  param = G3d_malloc (sizeof (G3d_paramType));

  param->type = G_define_option();
  param->type->key = "type";
  param->type->type = TYPE_STRING;
  param->type->required = NO;
  param->type->multiple = NO;
  param->type->answer = "default";
  param->type->description = "Data type used in the output file";
  param->type->options = "default,double,float";

  param->precision = G_define_option();
  param->precision->key = "precision";
  param->precision->type = TYPE_STRING;
  param->precision->required = NO;
  param->precision->multiple = NO;
  param->precision->answer = "default";
  param->precision->description = 
    "Precision used in the output file (default, max, or 0 to 52)";

  param->compression = G_define_option();
  param->compression->key = "compression";
  param->compression->type = TYPE_STRING;
  param->compression->required = NO;
  param->compression->multiple = NO;
  param->compression->answer = "default";
  param->compression->description = 
    "The compression method used in the output file";
  param->compression->options = "default,rle,lzw,rle+lzw,none";

  param->dimension = G_define_option();
  param->dimension->key = "tiledimension";
  param->dimension->type = TYPE_STRING;
  param->dimension->required = NO;
  param->dimension->multiple = NO;
  param->dimension->answer = "default";
  param->dimension->description = 
    "The dimension of the tiles used in the output file";
}

/*----------------------------------------------------------------------------*/

int
G3d_getStandart3dParams (useTypeDefault, type, 
			 useLzwDefault, doLzw, 
			 useRleDefault, doRle, 
			 usePrecisionDefault, precision,
			 useDimensionDefault, tileX, tileY, tileZ)

     int *useTypeDefault, *type, *useLzwDefault, *doLzw, *useRleDefault;
     int *doRle, *usePrecisionDefault, *precision, *useDimensionDefault;
     int *tileX, *tileY, *tileZ;

{
  int doCompress;

  *useTypeDefault = *useLzwDefault = *useRleDefault = 0;
  *usePrecisionDefault = *useDimensionDefault = 0;

  G3d_initDefaults ();

  if (strcmp (param->type->answer, "double") == 0)
    *type = G3D_DOUBLE;
  else if (strcmp (param->type->answer, "float") == 0)
    *type = G3D_FLOAT;
  else {
    *type = G3d_getFileType ();
    *useTypeDefault = 1;
  }

  G3d_getCompressionMode (&doCompress, doLzw, doRle, precision);

  if (strcmp (param->precision->answer, "default") != 0) {
    if (strcmp (param->precision->answer, "max") == 0) 
      *precision = -1;
    else
      if ((sscanf (param->precision->answer, "%d", precision) != 1) ||
	  (*precision < 0)) {
	G3d_error ("G3d_getStandart3dParams: precision value invalid");
	return 0;
      }
  } else
    *usePrecisionDefault = 1;


  if (strcmp (param->compression->answer, "default") != 0) {
    if (strcmp (param->compression->answer, "rle") == 0) {
      *doRle = G3D_USE_RLE;
      *doLzw = G3D_NO_LZW;
    } else if (strcmp (param->compression->answer, "lzw") == 0) {
      *doRle = G3D_NO_RLE;
      *doLzw = G3D_USE_LZW;
    } else if (strcmp (param->compression->answer, "rle+lzw") == 0) {
      *doRle = G3D_USE_RLE;
      *doLzw = G3D_USE_LZW;
    } else {
      *doRle = G3D_NO_RLE;
      *doLzw = G3D_NO_LZW;
    }
  } else 
    *useLzwDefault = *useRleDefault = 1;

  G3d_getTileDimension (tileX, tileY, tileZ);
  if (strcmp (param->dimension->answer, "default") != 0) {
    if (sscanf (param->dimension->answer, "%dx%dx%d", 
		tileX, tileY, tileZ) != 3) {
      G3d_error ("G3d_getStandart3dParams: tile dimension value invalid");
      return 0;
    }
  } else
    *useDimensionDefault = 1;

  G3d_free (param);
  
  return 1;
}

/*----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/

static struct Option *windowParam = NULL;

void
G3d_setWindowParams ()

{
  windowParam = G_define_option();
  windowParam->key = "region3";
  windowParam->type = TYPE_STRING;
  windowParam->required = NO;
  windowParam->multiple = NO;
  windowParam->answer = NULL;
  windowParam->description = "Window replacing the default.";
}

/*----------------------------------------------------------------------------*/

char * G3d_getWindowParams ()

{
  if (windowParam == NULL) return NULL;
  if (windowParam->answer == NULL) return NULL;
  if (strcmp (windowParam->answer, G3D_WINDOW_ELEMENT) == 0) 
    return G_store (G3D_WINDOW_ELEMENT);
  return G_store (windowParam->answer);
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
