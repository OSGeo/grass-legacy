/******************************************************************************
 * main.c [s.in.mif]
 * Import MapInfo MIF/MID pair

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 10th. Aug. 2000
 * Last updated 11th. Oct. 2000
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
#include "site.h"
#include "local_structs.h"
#include "scanner.h"


/* Globals */
/* Mostly used in the scanner source file */

struct line_pnts *lrep;
site_array *site0;

double *xcenter;
double *ycenter;
int ncenter;
int numcols;
int has_mid;
int recs;
int nsites;

FILE *mif_file;
FILE *mid_file;

/* Array globals */

d_type version_no;
d_type chset;
d_type proj_info;
d_type *data_info;
field_data field_info;
field_data field_type;

extern d_type del0;
extern char delchar;

extern FILE *yyin;
extern int yylex();


static char *extract_base_name( char *, const char * );

int main(int argc, char *argv[] ) {

  /* Body of main function called from the `real' main() in the scanner source */

  /* Local */

  FILE *logfp = NULL;
  char basename[128], mif_input[128], mid_input[128];
  char slname[128];


  /* Main structures */

  struct {
    struct Option *input, *logfile, *sitefile;
  } parm;

  struct Flag *listflag, *rejflag;

  struct GModule *module;

  /* Are we running in Grass environment ? */

  G_gisinit (argv[0]);

  /* add module description, al 11/2000 */

  module = G_define_module();

  module->description = "Import MapInfo point data files into GRASS sites list file."; 

  /* define the different options */

  parm.input = G_define_option() ;
  parm.input->key        = "in";
  parm.input->type       = TYPE_STRING;
  parm.input->required   = YES;
  parm.input->description= "Name of .MIF/.MID file to be imported";

  parm.logfile = G_define_option() ;
  parm.logfile->key        = "logfile";
  parm.logfile->type       = TYPE_STRING;
  parm.logfile->required   = NO;
  parm.logfile->description= "Name of file where log operations";
  parm.logfile->answer     = "mif.log";

  parm.sitefile = G_define_option();
  parm.sitefile->key        = "out";
  parm.sitefile->type       = TYPE_STRING;
  parm.sitefile->required   = NO;
  parm.sitefile->description= "Name of sites-list file";
  parm.sitefile->answer     = "";
  
    

  /* Set flag for listing fields of database */

  listflag = G_define_flag();
  listflag->key     = 'l';
  listflag->description = "List fields of coverage";

  /* get options and test their validity */

  if (G_parser(argc, argv))
    exit(-1);

  /* Get base name. Should be .mif or .mid of the pair or
     .mif only
  */

  extract_base_name( basename, parm.input->answer );

  strcpy(mif_input, basename);
  strcat(mif_input, ".mif");

  strcpy(mid_input, basename);
  strcat(mid_input, ".mid");


  /* Obtain the name of the site-list  */

  if( strcmp(parm.sitefile->answer, "" ) == 0 )
    strcpy(slname, basename);
  else
    strcpy(slname, parm.sitefile->answer);
  
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


  /* Open mid-file */

  if(has_mid) {
    if( (mid_file = fopen(mid_input, "r")) == NULL ) {
      fprintf(stderr, "Unable to open file `%s' for read. Continuing with no data.\n",
	      mid_input);
      has_mid = 0;
    }
  }
  
  else mid_file = NULL;

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

  /* Now the main business */

  yyin = mif_file;
  yylex();

  /* assert(recs == nsites); */


  /* Now pass information to site-list */

  sites_write(site0, slname, data_info, del0, nsites, logfp);


  /* Now dump information to log file */

  /* sites_dumpall(logfp, site0, data_info, del0, nsites); */


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
