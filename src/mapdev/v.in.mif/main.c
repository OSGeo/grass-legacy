/******************************************************************************
 * main.c [v.in.mif]
 * Import MapInfo MIF/MID pair

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 10th. Aug. 2000
 * Last updated 14th. Jan. 2001
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/

/* Includes macros etc */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "gis.h"
#include "Vect.h"
#include "local_structs.h"
#include "scanner.h"
#include "store.h"
#include "cpoly.h"
#include <stdio.h>

/* Globals */
/* Mostly used in the scanner source file */

struct line_pnts *lrep;
site_array *site0;  /* Deals with collecting point data */
line_array *line0;  /* Deals with collecting line data  */
line_array *area0;  /* Deals with collecting area data  */
type_array *type0;
ring_offsets *roffs;

double *xcenter;
double *ycenter;
int ncenter;
int numcols;
int has_mid;
int recs;
int nsites;
int nglines;
int nrings;
int site_index;
int line_index;
int region_index;
int line_grps;
int ring_grps;

FILE *mif_file;
FILE *mid_file;

int header_only;

/* Array globals */

d_type version_no;
d_type chset;
d_type proj_info;
d_type *data_info;
field_data field_info;
field_data field_type;

extern d_type del0;
extern char delchar;
extern region uni_bb;

extern FILE *yyin;
extern int yylex();


static char *extract_base_name( char *, const char * );

int main(int argc, char *argv[] ) {

  /* Body of main function called from the `real' main() in the scanner source */

  /* Local */

  FILE *logfp = NULL;
  char basename[128], mif_input[128], mid_input[128];
  char attname[128], outfile[128];
  char digits_buf[512];

  int scale0;
  double snap_default = 0.000001;
  double tol_default = 1.745e-4;
  int scale_default = 200;
  double snap, tol;
  double false_easting, false_northing;
  int idigits;


  /* Main structures */

  struct {
    struct Option *input, *output, *logfile, *verbose, *attribute, *snapd, *minangle;
    struct Option *scale, *catlabel;
  } parm;

  struct Flag *listflag, *rejflag;

  /* Are we running in Grass environment ? */

  G_gisinit (argv[0]);

  /* define the different options */

  parm.input = G_define_option() ;
  parm.input->key        = "input";
  parm.input->type       = TYPE_STRING;
  parm.input->required   = YES;
  parm.input->description= "Name of .MIF/.MID file to be imported";

  parm.output = G_define_option() ;
  parm.output->key        = "output";
  parm.output->type       = TYPE_STRING;
  parm.output->required   = YES;
  parm.output->gisprompt  = "new,dig,vector";
  parm.output->description= "Name of target vector file";

  parm.logfile = G_define_option() ;
  parm.logfile->key        = "logfile";
  parm.logfile->type       = TYPE_STRING;
  parm.logfile->required   = NO;
  parm.logfile->description= "Name of file where log operations";
  parm.logfile->answer     = "";

  parm.snapd = G_define_option() ;
  parm.snapd->key        = "snapdist";
  parm.snapd->type       = TYPE_STRING;
  parm.snapd->required   = NO;
  parm.snapd->description= "Snap distance in ground units (Default = 0.000001)";
  parm.snapd->answer     = "0.000001";

  parm.minangle = G_define_option() ;
  parm.minangle->key        = "sliver";
  parm.minangle->type       = TYPE_STRING;
  parm.minangle->required   = NO;
  parm.minangle->description= "Min. angle subtended by a wedge at node (radians)";
  parm.minangle->answer     = "1.745e-4"; 

  parm.scale = G_define_option() ;
  parm.scale->key        = "scale";
  parm.scale->type       = TYPE_STRING;
  parm.scale->required   = NO;
  parm.scale->description= "Set initial scale [1:200]";
  parm.scale->answer     = "200";

  parm.attribute = G_define_option() ;
  parm.attribute->key        = "attribute";
  parm.attribute->type       = TYPE_STRING;
  parm.attribute->required   = NO;
  parm.attribute->description= "Name of attribute to use as category";
  parm.attribute->answer     = "";
    

  /* parm.catlabel = G_define_option() ;
     parm.catlabel->key        = "label";
     parm.catlabel->type       = TYPE_STRING;
     parm.catlabel->required   = NO;
     parm.catlabel->description= "Name of attribute to use as category label";
     parm.catlabel->answer     = "";
  */
    

    /* Set flag for listing fields of database */

  listflag = G_define_flag();
  listflag->key     = 'l';
  listflag->description = "List fields of coverage";

  /* get options and test their validity */

  if (G_parser(argc, argv))
    exit(-1);


  /* Deal with the list flag first */

  if(listflag->answer)
    header_only = 1;
  else
    header_only = 0;

  /* Get base name. Should be .mif or .mid of the pair or
     .mif only
  */

  extract_base_name( basename, parm.input->answer );

  strcpy(mif_input, basename);
  strcat(mif_input, ".mif");

  strcpy(mid_input, basename);
  strcat(mid_input, ".mid");


  /* Find name of attribute field */

  if( strcmp(parm.attribute->answer, "" ) )
    strcpy(attname, parm.attribute->answer);
  else
    strcpy(attname, "__NO_FIELD__");


  /* Process the snap distance */

  if(strcmp(parm.snapd->answer, "") == 0) {
    /* set default value */

    if( G_process_snap_distance(SET_VAL, &snap_default) < 0 )
      G_warning("Unable to store snap distance value");
  }

  else {
    snap = atof(parm.snapd->answer);
    if( G_process_snap_distance(SET_VAL, &snap) < 0 )
      G_warning("Unable to store snap distance value");
  }

  /* Process the angular tolerance */

  if(strcmp(parm.minangle->answer, "") == 0) {
    /* set default value */

    if( G_process_colinear_tolerance(SET_VAL, &tol_default) < 0 )
      G_warning("Unable to store snap distance value");
  }

  else {
    tol = atof(parm.minangle->answer);
    if( G_process_colinear_tolerance(SET_VAL, &tol) < 0 )
      G_warning("Unable to store colinear tolerance value");
  }



  /* Store the initial scale */

  if(strcmp(parm.scale->answer, "") == 0) {
    /* set default value */

    if( G_process_scale_value(SET_VAL, &scale_default) < 0 )
      G_warning("Unable to set specified initial scale value");
  }

  else {
    scale0 = atoi(parm.scale->answer);
    if( G_process_scale_value(SET_VAL, &scale0) < 0 )
      G_warning("Unable to store colinear tolerance value");
  }

  
  /* Check for mif-file */

  if( 0 > access(mif_input, F_OK)) {
    fprintf(stderr, "Input .mif file does not exist. Aborting.\n");
    exit(-1);
  }
  
  if( 0 > access(mif_input, R_OK)) {
    fprintf(stderr, "Input .mif file cannot be read. Aborting.\n");
    exit(-1);
  }

  /* Open mif-file */

  if( (mif_file = fopen(mif_input, "r")) == NULL ) {
    fprintf(stderr, "Unable to open file `%s' for read. Aborting.\n",
	    mif_input);
    exit(-1);
  }

  
  /* Check for mid-file */

  if( 0 > access(mid_input, F_OK)) {
    fprintf(stderr, "WARNING: No data (.mid) file, continuing with no data\n");
    has_mid = 0;
  }
  
  if( 0 > access(mid_input, R_OK)) {
    fprintf(stderr, "WARNING: Data (.mid) file cannot be read. Continuing with no data\n");
    has_mid = 0;
  }

  else has_mid = 1;

  /* ...in passing... If there is no MID file we can't check the fields */

  if( !has_mid && listflag->answer ) {
    fprintf(stderr, "MID (data) file not available.\n");
    if(mif_file) fclose(mif_file);
    return 0;
  }


  /* Open mid-file */

  if(has_mid) {
    if( (mid_file = fopen(mid_input, "r")) == NULL ) {
      fprintf(stderr, "Unable to open file `%s' for read. Continuing with no data.\n",
	      mid_input);
      has_mid = 0;
    }
  }
  
  else mid_file = NULL;



  /* Output */

  if(G_legal_filename(parm.output->answer) )
    strcpy(outfile, parm.output->answer);
  else {
    fprintf(stderr, "Name of output file `%s' is illegal. Aborting.\n",
	    parm.output->answer);
    exit(-1);
  }

  /* Open log-file for write if it is required */

  if( strcmp(parm.logfile->answer, "") ) {

    if( (logfp = fopen( parm.logfile->answer, "w" )) == NULL )
      fprintf(stderr, "WARNING: Could not open log file for writing.\n");
  }

  /* At this stage abort if logfile is null as we need it */

  if( logfp == NULL )
    exit(-1);

  
  /* Initialise global structures required for lexer */

  site0 = (site_array *)G_malloc( sizeof(site_array) );
  line0 = (line_array *)G_malloc( sizeof(line_array) );
  area0 = (line_array *)G_malloc( sizeof(line_array) );

  /* Now the main business */

  yyin = mif_file;
  yylex();


  /* Process the key generation params. This must be done after
     the input files are parsed
  */

  if(uni_bb.w < 0.0) {
    false_easting = - ((long long)(uni_bb.w / snap) - 3) * snap;
  }

  else {
    false_easting = - ((long long)(uni_bb.w / snap) - 2) * snap;
  }

  if(uni_bb.s < 0.0) {
    false_northing = - ((long long)(uni_bb.s / snap) - 3) * snap;
  }

  else {
    false_northing = - ((long long)(uni_bb.s / snap) - 2) * snap;
  }

  snprintf(digits_buf, 16, "%ld", (long)(uni_bb.e + false_easting) + 2 );
  idigits = strlen(digits_buf);

  snprintf(digits_buf, 16, "%ld", (long)(uni_bb.n + false_northing) + 2 );
  if(strlen(digits_buf) > idigits) idigits = strlen(digits_buf);

  if(idigits > 16)
    G_fatal_error("Values of co-ordinate locations are outside viable range.");

  if(G_process_key_params(SET_VAL, &idigits, &false_easting, &false_northing) < 0) {
    G_warning("Unable to store regional parameters for key generation");
  }

  /* Deal with the listflag first */

  if(listflag->answer) {
    dump_field_info();
    if(mif_file) fclose(mif_file);
    if(mid_file) fclose(mid_file);
    if(logfp) fclose(logfp);
    return 0;
  }

  /* assert(recs == nsites); */


  /* Now write lines and data */

  line_data_write(site0, line0, area0, roffs, data_info, del0, nsites, nglines, nrings,
		  outfile, logfp, attname);


  /* Clean up */

  if(logfp) fclose(logfp);
  if(mid_file) fclose (mid_file);
  fclose(mif_file);

  return 0;
}





static char *extract_base_name( char *base_name, const char *in_name ) {
  
  /* Extract the basename of a fullname of the input file. ie. the answer to
     the option `in'
  */

  char *name;
  char *p;

  name = (char *)malloc(strlen(in_name)+1);
  strcpy( name, in_name );

  for( p = name+strlen(name)-1;
       p != name-1 && (isalnum(*p) || *p == '_' || *p == '.' );
       p-- ) {}
  strcpy( base_name, p+1);
  free(name);
    
  p = strrchr( base_name, '.');
  if (p != NULL)
    *p = '\0';
  base_name = p;
  return base_name;
}
