/*
 * r3.out.ascii -
 *
 * Mark Astley
 * USA CERL started 4/4/96
 * See the file "ChangeLog" for changes and revisions
 *
 */

#include <gis.h>
#include "G3d.h"
#include <stdio.h>

#define MAX(a,b) (a > b ? a : b)

/* structs */
typedef	struct {
  struct Option *input, *output, *decimals, *null_val;
  struct Flag *header;
} paramType;

/* protos */
void fatalError(char *errorMsg);
void setParams();
void getParams(char **input, char **output, int *decim);
void writeHeaderString(FILE *fp, char *valueString, double value);
void writeHeaderString2(FILE *fp, char *valueString, int value);
FILE *openAscii(char *asciiFile, G3D_Region region);
void G3dToascii(FILE *fp, G3D_Region *region, int decim);

/* globals */
void *map = NULL;
paramType param;

/*---------------------------------------------------------------------------*/
/* Simple error handling routine, will eventually replace this with
 * G3D_fatalError.
 */
void fatalError(char *errorMsg) {
  if (map != NULL) {
    /* should unopen map here! */
  }
  
  G3d_fatalError (errorMsg);
}

/*---------------------------------------------------------------------------*/
/* Convenient way to set up the arguments we are expecting
 */
void setParams() {
  param.input = G_define_option();
  param.input->key = "map";
  param.input->type = TYPE_STRING;
  param.input->required = YES;
  param.input->gisprompt = "old,grid3,3d-raster";
  param.input->multiple = NO;
  param.input->description = "G3d raster map to be converted to ASCII";
  
  param.output = G_define_option();
  param.output->key = "output";
  param.output->type = TYPE_STRING;
  param.output->required = NO;
  param.output->description = "Name for ascii output file";

  param.decimals = G_define_option();
  param.decimals->key = "dp";
  param.decimals->type = TYPE_INTEGER;
  param.decimals->required = NO;
  param.decimals->multiple = NO;
  param.decimals->answer = "8";
  param.decimals->options = "0-20";
  param.decimals->description = "Number of decimal places for floats";

  param.null_val = G_define_option();
  param.null_val->key = "null";
  param.null_val->type = TYPE_STRING;
  param.null_val->required = NO;
  param.null_val->description = "Char string to represent no data cell";
  param.null_val->answer = "*";

  param.header = G_define_flag();
  param.header->key = 'h';
  param.header->description = "Suppress printing of header information";
}

/*---------------------------------------------------------------------------*/
/* Set up the input and output file names from the user's responses
 */
void getParams(char **input, char **output, int *decim) {
  *input = param.input->answer;
  *output = param.output->answer; 
  sscanf(param.decimals->answer, "%d", decim);
}

/*---------------------------------------------------------------------------*/
/* This function is used to write parts of the header for the output
 * ASCII file.
 */ 
void writeHeaderString(FILE *fp, char *valueString, double value) {
  static char format[100];

  sprintf (format, "%s %%lf\n", valueString);
  if (fprintf (fp, format, value) < 0)
    fatalError ("writeHeaderString: header value invalid");
}

void writeHeaderString2(FILE *fp, char *valueString, int value) {
  static char format[100];

  sprintf (format, "%s %%d\n", valueString);
  if (fprintf (fp, format, value) < 0)
    fatalError ("writeHeaderString: header value invalid");
}

/*---------------------------------------------------------------------------*/
/* Opens the output acsii file and writes the header.
 * Returns the file handle for the output file.
 */
FILE *openAscii(char *asciiFile, G3D_Region region) {
  FILE *fp;

  if (asciiFile) {
    fp = fopen(asciiFile, "w");
    if (fp == NULL) {
      perror(asciiFile);
      G_usage();
      exit(-1);
    }
  } else
    fp=stdout;

  if (!param.header->answer) {
    writeHeaderString(fp, "north:", region.north);
    writeHeaderString(fp, "south:", region.south);
    writeHeaderString(fp, "east:", region.east);
    writeHeaderString(fp, "west:", region.west);
    writeHeaderString(fp, "top:", region.top);
    writeHeaderString(fp, "bottom:", region.bottom);
    writeHeaderString2(fp, "rows:", region.rows); 
    writeHeaderString2(fp, "cols:", region.cols); 
    writeHeaderString2(fp, "levels:", region.depths); 
  }
  
  return fp;
}

/*---------------------------------------------------------------------------*/
/* This function does all the work.  Basically, we just output the
 * source G3d file one layer at a time.
 */
void G3dToascii(FILE *fp, G3D_Region *region, int decim) {
  double d1 = 0;
  double *d1p;
  float *f1p;
  int x, y, z;
  int rows, cols, depths, typeIntern;

  G3d_getCoordsMap (map, &rows, &cols, &depths);
  typeIntern = G3d_tileTypeMap (map);

  d1p = &d1; f1p = (float *) &d1;

  for (z = 0; z < depths; z++) 
    for (y = rows-1; y >= 0; y--) {    /* north to south */
      for (x = 0; x < cols; x++) {
	G3d_getValue (map, x, y, z, d1p, typeIntern);
	if (typeIntern == G3D_FLOAT) {
	  if (G3d_isNullValueNum(f1p, G3D_FLOAT))
	    fprintf (fp, "%s ", param.null_val->answer);
	  else
	    fprintf (fp, "%.*f ", decim, *f1p);
	} else {
	  if (G3d_isNullValueNum(&d1, G3D_DOUBLE))
	    fprintf (fp, "%s ", param.null_val->answer);
	  else
	    fprintf (fp, "%.*lf ", decim, d1);
	}
      }
      fprintf (fp, "\n");
    }
}

/*---------------------------------------------------------------------------*/
/* Main function: open the input and output files, then call
 * G3dtoascii.
 */
int main(int argc, char *argv[]) {
  char *input, *output;
  int convertNull, decim;
  double nullValue;
  int useTypeDefault, type, useLzwDefault, doLzw, useRleDefault, doRle;
  int usePrecisionDefault, precision, useDimensionDefault, tileX, tileY, tileZ;
  G3D_Region region;
  FILE *fp;
  int cacheSize;

  /* Initialize GRASS */
  G_gisinit(argv[0]);

  /* Get parameters from user */
  setParams();

  /* Have GRASS get inputs */
  if (G_parser(argc, argv))
    exit(-1);

  /* Parse input parameters */
  getParams(&input, &output, &decim);

/*  map = G3d_openCellOld(input, G_find_grid3(input, ""), G3D_DEFAULT_WINDOW,
			G3D_TILE_SAME_AS_FILE,
			G3D_NO_CACHE);*/
  /* using cache mode due to bug */			
  map = G3d_openCellOld(input, G_find_grid3(input, ""), G3D_DEFAULT_WINDOW,
			G3D_TILE_SAME_AS_FILE,
			G3D_USE_CACHE_DEFAULT);			
  if (map == NULL)
    G3d_fatalError("main: error opening g3d file");

  /* Figure out the region from the map */
/*  G3d_getRegionStructMap(map, &region);*/
    G3d_getWindow(&region);

  /* Open the output ascii file */
  fp = openAscii(output, region);

  /* Now barf out the contents of the map in ascii form */
  G3dToascii(fp, &region, decim);

  /* Close files and exit */
  if (!G3d_closeCell (map)) 
    fatalError ("main: error closing new g3d file");

  map = NULL;
  if (output)
    if (fclose (fp))
      fatalError ("main: error closing ascii file");
}

/*---------------------------------------------------------------------------*/
