/*
 * r3.out.sites -
 * 
 * developed from r3.out.ascii by
 * Mark Astley
 * USA CERL started 4/4/96
 * See the file "ChangeLog" for changes and revisions
 *
 */

#include "gis.h"
#include "G3d.h"
#include <stdio.h>
#include <strings.h>

typedef int FILEDESC;
double atof();

#define MAX(a,b) (a > b ? a : b)

/* structs */
typedef	struct {
  struct Option *input, *output, *decimals, *null_val, *lab;
  struct Flag *header;
} paramType;

 struct Option 	*rast, *sites, *lab;
 struct Flag 	*attr, *dim;
 char 		errbuf[100];
 FILEDESC    	cellfile = (FILEDESC) NULL;
 struct Cell_head    w;
 int                 mtype;
   
/* protos */
void fatalError(char *errorMsg);
void setParams();
void getParams(char **input, char **output, int *decim);
void writeHeaderString(FILE *fp, char *valueString, double value);
void writeHeaderString2(FILE *fp, char *valueString, int value);
void G3dToSites(FILE *sitesfile, G3D_Region *region, int decim);
FILE                *sitesfile = NULL;
Site                *s, *sa, *sd;

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
  param.input->key = "input";
  param.input->type = TYPE_STRING;
  param.input->required = YES;
  param.input->gisprompt = "old,grid3,3d-raster";
  param.input->multiple = NO;
  param.input->description = "G3d raster map to be converted to sites map";
  
  param.output = G_define_option();
  param.output->key = "output";
  param.output->type = TYPE_STRING;
  param.output->required = YES;
  param.output->gisprompt = "new,site_lists,sites";
  param.output->description = "Name of new sites file";

  param.decimals = G_define_option();
  param.decimals->key = "decimals";
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

    lab = G_define_option();
    lab->key                    = "label";
    lab->type                   = TYPE_STRING;
    lab->required               = NO;
    lab->description            = "Label for site values";
    lab->answer                 = "No Label";
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
void G3dToSites(FILE *sitesfile, G3D_Region *region, int decim) {
  double d1 = 0;
  double *d1p;
  float *f1p;
  int x, y, z;
  int rows, cols, depths, typeIntern;

  G3d_getCoordsMap (map, &rows, &cols, &depths);
  typeIntern = G3d_tileTypeMap (map);

  d1p = &d1; f1p = (float *) &d1;

   {
    Site_head shead;
    DateTime dt;
    struct TimeStamp ts;
    int tz, ret;
    char buf[320];

	shead.name = G_store(sites->answer);
	shead.desc = G_store(G_recreate_command());
        shead.time = (struct TimeStamp*)NULL;

	shead.form = shead.labels = (char *)NULL;

	if(attr->answer){
	    shead.form = G_store("||%");
	    sprintf(buf,"Easting|Northing|%%%s",lab->answer);
	    shead.labels = G_store(buf);
	}
	else if (dim->answer){
	    shead.form = G_store("|||");
	    sprintf(buf,"Easting|Northing|%s|",lab->answer );
	    shead.labels = G_store(buf);
	}
	else{
	    shead.form = G_store("||#");
	    sprintf(buf,"Easting|Northing|#%s",lab->answer );
	    shead.labels = G_store(buf);
	}

	G_site_put_head (sitesfile, &shead);
	free (shead.name);
	free (shead.desc);
	free (shead.form);
	free (shead.labels);
    }

  for (z = 0; z < depths; z++) 
    for (y = rows-1; y >= 0; y--) {    /* north to south */
      for (x = 0; x < cols; x++) {
	G3d_getValue (map, x, y, z, d1p, typeIntern);
	
/*	if (typeIntern == G3D_FLOAT) {
	  if (G3d_isNullValueNum(f1p, G3D_FLOAT))
	    sa->dbl_att[0]= (float)param.null_val->answer;
	  else
	     sa->dbl_att[0] = *f1p;
	} else {
	  if (G3d_isNullValueNum(&d1, G3D_DOUBLE))
	    sa->dbl_att[0]= (float)param.null_val->answer;
	  else
	    sa->dbl_att[0] = d1;
	}
*/
        sa->dbl_att[0] = *f1p;
      }
    G_site_put(sitesfile, sa);
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

  map = G3d_openCellOld(input, G_find_grid3(input, ""), G3D_DEFAULT_WINDOW,
			G3D_TILE_SAME_AS_FILE,
			G3D_NO_CACHE);
  if (map == NULL)
    G3d_fatalError("main: error opening g3d file");

    if ((sitesfile = G_fopen_sites_new(param.output->answer)) == NULL)
    {
	sprintf(errbuf,"Not able to open sitesfile for [%s]", param.output->answer);
	G_fatal_error(errbuf);
    }
     G_get_set_window (&w);

       if(NULL == (s = G_site_new_struct(mtype,2,0,0)))
	G_fatal_error("memory allocation failed for site");

    if(NULL == (sa = G_site_new_struct(-1,2,0,1)))
	G_fatal_error("memory allocation failed for site");

    if(NULL == (sd = G_site_new_struct(-1,3,0,0)))
	G_fatal_error("memory allocation failed for site");

 
  /* Figure out the region from the map */
/*  G3d_getRegionStructMap(map, &region);*/
    G3d_getWindow(&region);

  /* Now barf out the contents of the map in ascii form */
  G3dToSites(fp, &region, decim);

  /* Close files and exit */
  if (!G3d_closeCell (map)) 
    fatalError ("main: error closing  g3d file");

  map = NULL;

    G_site_free_struct (s);
    G_site_free_struct (sa);
    G_site_free_struct (sd);
    G_close_cell(cellfile);
    if (fclose(sitesfile))
      fatalError ("main: error closing sites file");

  return(0);
}

