/******************************************************************************
 * main.c <s.in.shape>
 * import shapefile points file to GRASS sites file

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 12th. Apr. 2000
 * Last updated 31st. May. 2000
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

#include <stdio.h>
#include <stdlib.h>
/* #include <unistd.h> */
#include <string.h>
#include <math.h>
/* #include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h> */
#include <assert.h>
#include <time.h>
#include "gis.h"
#include "site.h"
/* #include "Vect.h" */
#include "shapefil.h"
#include "s_procs.h"


/* Requires Frank Warmerdam's shapelib v. 1.2.8 or later */

static char *get_base_name( char *, const char * );



int main( int argc, char *argv[] ) {

  /* Main command-line procedures for shapefile import to sites list */

  /* Loop variables */
  int i, j, j1;
  int sFieldCount, fFieldCount;

  /* Principal structures */
  SHPHandle hShape;
  DBFHandle hDBF;
  fieldDescriptor *fdd;
  aggregateList *aggd0;
  Site *hSite;
  Site_head *hHead;

  /* Parser stuff */
  struct Option *pnt_input, *pnt_att, *pnt_height;
  struct Flag *listflag, *zflag, *mflag;
  struct GModule *module;
  
  /* Height handling values */
  int hasz;
  double zval;

  /* Measure handling values */
  int hasm;

  /* Various flags and descriptors */
  int nFields, nRecs, isatt, isfp = 0, att_field;
  int type0;
  int dummy1, dummy2;
  DBFFieldType tmp_type;
  FILE *fSite;
  int nNumeric, nString, nInt; /* Number of float, string attributes */

  /* Useful buffers */
  char mapname[512], infile[512], tmp_namebuf[512], buf0[512], ts_buf[128];


  /* Time and date stuff */

  struct tm *site_time;
  struct TimeStamp *ts0;
  time_t t0, t1;
  DateTime *dt0;

  /* Initialise parser with options from command line */
  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                "Read an ArcView Shapefile with points or multipoint "
                "shapes";
                
  /* Options */

  pnt_input = G_define_option() ;
  pnt_input->key        = "in";
  pnt_input->type       = TYPE_STRING;
  pnt_input->required   = YES;
  pnt_input->description= "Name of sites file to be imported";


  pnt_att = G_define_option() ;
  pnt_att->key        = "attribute";
  pnt_att->type       = TYPE_STRING;
  pnt_att->required   = NO;
  pnt_att->description= "Name of attribute field";
  pnt_att->answer     = "";
    
  pnt_height = G_define_option() ;
  pnt_height->key        = "height";
  pnt_height->type       = TYPE_STRING;
  pnt_height->required   = NO;
  pnt_height->description= "Optional fixed scalar height";
  pnt_height->answer     = "0";
    

  /* Set flag for listing fields of database */

  listflag = G_define_flag();
  listflag->key     = 'l';
  listflag->description = "List fields of DBF file";

  zflag = G_define_flag();
  zflag->key     = 'z';
  zflag->description = "Supply height field for site map";

  mflag = G_define_flag();
  mflag->key     = 'm';
  mflag->description = "Supply measure field for site map";


  /* process options */

  if (G_parser(argc, argv))
    exit(-1);

  /* Do we want to include a scalar height field? */

  if(zflag->answer) 
    hasz = 1;
  else
    hasz = 0;

  zval = atof( pnt_height->answer ); /* If the answer wasn't sensible, let it
					default to 0
				     */
			    
  /* Do we want to include a measure field? */

  if(mflag->answer) 
    hasm = 1;
  else
    hasm = 0;



  /* Get the base name of the input file */

  strncpy( infile, pnt_input->answer, 511 );
  get_base_name( mapname, pnt_input->answer );


  /* Open shape, dbf files for input */

  if( (hShape = SHPOpen( infile, "rb" )) == NULL )
    G_fatal_error( "Unable to open shapefile - Aborting." );
  if( (hDBF = DBFOpen( infile, "rb" )) == NULL ) {
    SHPClose(hShape);
    G_fatal_error( "Unable to open dbf records file - Aborting. " );
  }

  type0 = hShape->nShapeType;
  if( hasm && (type0 == SHPT_POINTM || type0 == SHPT_MULTIPOINTM ||
	       type0 == SHPT_POINTZ || type0 == SHPT_MULTIPOINTZ) )
    hasm = 1;
  else
    hasm = 0;

  /* Do we just want a list of available fields in dbf? */

  if(listflag->answer) {
    int	i;        

    fprintf (stdout, "Attributes available in %s\n", mapname );
    for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
      {
	char	field_name[15];

	DBFGetFieldInfo( hDBF, i, field_name, NULL, NULL );
	fprintf (stdout, "%s\n", field_name );
      }
        
    DBFClose( hDBF );

    exit( 0 );
  }

  /* Get number of fields */

  nFields = DBFGetFieldCount( hDBF ); /* x, y, [z], id, origid, [m] */
  fdd = (fieldDescriptor *)malloc( (nFields + 6) * sizeof(fieldDescriptor));

  /* Are we going to use a category attribute field - or just
     assign an ID from the order of the record?
  */

  isatt = -1;
  att_field = -1;
  if( strcmp(pnt_att->answer, "") == 0 )
    isatt = 0;
  else {
    for( i = 0; i < nFields; ++i ) {
      tmp_type = DBFGetFieldInfo( hDBF, i, tmp_namebuf, &dummy1, &dummy2 );
      if( strcmp(pnt_att->answer, tmp_namebuf) == 0 ) {
	att_field = i;
	break;
      }
    }
  }

  if( isatt < 0 ) {
    if( att_field < 0 ) {
      G_warning( "No field called `%s' found in database - using ID number instead.",
		 pnt_att->answer );
      isatt = 0;
    }
    else if( tmp_type != 1 && tmp_type != 2) {
      G_warning( "Assigned category field is not numeric type - using ID number instead." );
      att_field = -1;
      isatt = 0;
    }
    else {
      isatt = 1;
      if( tmp_type == 1 )
	isfp = 0;
      else
	isfp = 1;
    }
  }

  if(att_field >= 0) att_field += 6;


  /* Initialise aggregate list */

  aggd0 = (aggregateList *)malloc( sizeof(aggregateList) );
  aggd0->numaggs = hShape->nRecords;
  aggd0->aggs = (aggregateDescriptor *)malloc(aggd0->numaggs * sizeof(aggregateDescriptor));


  /* The number of records should correspond to the number of shape entities */

  assert( hShape->nRecords == DBFGetRecordCount( hDBF ) );


  /* Process data. Load all information into a field descriptor */

  if( load_mpshape_data( hShape, aggd0, hDBF, fdd, &nFields ) != 0 )
    G_fatal_error( "Unable to process shapefile - Aborting." );


  /* Rearrange the field descriptors in the field descriptor list
     to facilitate identification of string and numerical fields
  */

  /* dbf_fields_reorder( fdd, &nFields );  */

  /* How many of each type of field is there? */

  nNumeric = nString = nInt = 0;
  for( i = 0; i < nFields; ++i ) {

    tmp_type = DBFGetFieldInfo( hDBF, i, tmp_namebuf, &dummy1, &dummy2 );
    
    switch(tmp_type) {
    case 0:
      {
	nString++;
	break;
      }
    case 1:
      {
	if( i != att_field )
	  nInt++;
	break;
      }
    case 2:
      {
	nNumeric++;
	break;
      }
    default:
      {
	nInt++;
      }
    }

    
  }

  nNumeric += nInt; /* Add the integer fields to the fp fields to get total numeric
		       fields
		    */

  nNumeric += 1 + hasm; /* Also the special numeric fields */


  /* How many records did we pick up in the original file? */

  nRecs = fdd[0].nRec;

  /* Open the sites file for writing */
  
  if( (fSite = G_sites_open_new( mapname )) == NULL )
    G_fatal_error( "Unable to open sites file `%s' for writing. Aborting.",
		   mapname );

  hHead = (Site_head *)malloc( sizeof(Site_head) );

  hHead->name = (char *)malloc( strlen(mapname) + 1 );
  strcpy( hHead->name, mapname );
  
  sprintf( buf0, "Imported from points shapefile %s.shp.", mapname );
  hHead->desc = (char *)malloc( strlen(buf0) + 1 );
  strcpy( hHead->desc, buf0 );

  /* Fix the time */

  t0 = time( &t1 );
  site_time = localtime(&t1);

  dt0 = (DateTime *)malloc( sizeof(DateTime) );
  dt0->mode = DATETIME_ABSOLUTE;
  dt0->year = site_time->tm_year;
  dt0->month = site_time->tm_mon;
  dt0->day = site_time->tm_mday;
  dt0->hour = site_time->tm_hour;
  dt0->minute = site_time->tm_min;
  dt0->second = (double)site_time->tm_sec;
  dt0->fracsec = 0;
  dt0->positive = 1;
  dt0->tz = 0;

  ts0 = (struct TimeStamp *)malloc( sizeof(struct TimeStamp) );
  G_set_timestamp( ts0, dt0 );

  hHead->time = ts0;
  G_format_timestamp( ts0, ts_buf );
  hHead->stime = (char *)malloc(128);
  strcpy( hHead->stime, ts_buf );

  G_site_put_head( fSite, hHead );


  /* Now loop through all these records and write a site record for each */

  for( i = 0; i < nRecs; ++i ) {

    sFieldCount = 0;
    fFieldCount = 0;

    /* Initialise a site structure */
    if( isatt == 0 || isfp == 0 ) {
      if( (hSite = G_site_new_struct( CELL_TYPE, hasz + 2, nString, nNumeric )) == NULL )
	G_fatal_error( "Unable to create site structure - Aborting." );
    }
    else {
      if( (hSite = G_site_new_struct( FCELL_TYPE, hasz + 2, nString, nNumeric )) == NULL )
	G_fatal_error( "Unable to create site structure - Aborting." );
    }

    /* Write easting and northing first */
    hSite->east = fdd[0].fldRecs[i].doubleField;
    hSite->north = fdd[1].fldRecs[i].doubleField;

    /* Deal with the height field */
    if(hasz) {
      if( type0 == SHPT_POINTZ || type0 == SHPT_MULTIPOINTZ )
	hSite->dim[0] = fdd[2].fldRecs[i].doubleField;
      else
	hSite->dim[0] = zval;
    }

    /* Write category value */
    if(isatt) {
      if(hSite->cattype == FCELL_TYPE)
	hSite->fcat = (float)fdd[att_field].fldRecs[i].doubleField;
      else
	hSite->ccat = fdd[att_field].fldRecs[i].intField;
    }
    else
      hSite->ccat = fdd[3].fldRecs[i].intField;

    /* Write multipoint index */

    hSite->dbl_att[fFieldCount++] = fdd[4].fldRecs[i].intField;

    /* Write measure value if required */

    if(hasm)
      hSite->dbl_att[fFieldCount++] = fdd[5].fldRecs[i].doubleField;


    /* Loop through fields to extract the data */
    j1 = 0;
    for( j = 0; j < nFields; ++j ) {
      if( isatt && j == att_field ) continue;
      if(fdd[j1+6].fldType == FTInteger)
	hSite->dbl_att[fFieldCount++] = fdd[j1+6].fldRecs[i].intField;
      else if(fdd[j1+6].fldType == FTDouble)
	hSite->dbl_att[fFieldCount++] = fdd[j1+6].fldRecs[i].doubleField;
      else
	strcpy( hSite->str_att[sFieldCount++], fdd[j1+6].fldRecs[i].stringField );
      j1++;
    }

    /* Write out the site structure to the sites list file */

    G_site_put( fSite, hSite );


    /* Dispose site structure */

    G_site_free_struct( hSite );
  }

  /* Close the sites list file */

  fclose(fSite);
  
  return(0);
}


static char *get_base_name( char *base_name, const char *in_name ) {
 
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
