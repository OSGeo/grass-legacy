/******************************************************************************
 * main.c [v.out.shape2]
 * export vector map as ESRI shapefile.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 29th. Jun. 2000
 * Last updated 2nd. Jul. 2000
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

#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include "shapefil.h"
#include "gis.h"
#include "Vect.h"
#include "local_structs.h"
#include "local_proto.h"

#define BOTH 8

#define STR_CAT 0
#define INT_CAT 1
#define FLT_CAT 2


int main( int argc, char *argv[] ) {


  /* Main module for export of ESRI shapefile */


  /* Main structures */
  struct GModule *module;
  struct Option *out, *set, *altname, *logfile, *shapetype, *category;
  struct Flag *chatflag;
  struct Map_info vmap;
  struct Categories *Cat1;
  fieldDescriptor *fd0, *fd1;
  DBFHandle db0, db1;
  char vecname[128], setname[128], shapename[128];
  char shapename_area[128], shapename_line[128], delfile[128];
  char errbuf[512];
  int docats, cat_type = 0;
  int shptype;
  int rec_size = 1000, rec_incr = 500;

  /* Data processing structures */
  int *poly_chklist, *line_chklist, *rev_index;
  int pcl_size = 1000, pcl_incr = 500;
  int lcl_size = 1000, lcl_incr = 500;
  SHPHandle hSA, hSL;
  SHPObject *hShape;
  int found = 1, where = 0;
  int nP = 0, nL = 0;
  int no, res;


  /* loop */

  int ia, ib;
  

  /* Initialise module */
  G_gisinit( argv[0] );

  module = G_define_module();
  module->description =
	"Export GRASS vector files to SHAPE";

  out = G_define_option();
  out->key = "map";
  out->type = TYPE_STRING;
  out->required = YES;
  out->description = "Vector map to be exported";

  shapetype = G_define_option();
  shapetype->key = "type";
  shapetype->type = TYPE_STRING;
  shapetype->required = YES;
  shapetype->description = "Type of coverage (area, line or both)";
  shapetype->options = "area,line,both";
  shapetype->answer = "";
    
  set = G_define_option();
  set->key = "mapset";
  set->type = TYPE_STRING;
  set->required = NO;
  set->description = "Mapset holding vector map to be exported (Default = current)";
  set->answer = "";

  category = G_define_option();
  category->key = "cats";
  category->type = TYPE_STRING;
  category->required = NO;
  category->description = "Type of field for category (none[default],string,integer,float)";
  category->options = "none,string,integer,float";
  category->answer = "none";

  altname = G_define_option();
  altname->key = "prefix";
  altname->type = TYPE_STRING;
  altname->required = NO;
  altname->description = "Specify alternative prefix for output files";
  altname->answer = "";

  logfile  = G_define_option();
  logfile->key = "log";
  logfile->type = TYPE_STRING;
  logfile->required = NO;
  logfile->description = "Logfile to hold results of output";
  logfile->answer  = "";


  /* Set flags */

  chatflag = G_define_flag();
  chatflag->key = 'v';
  chatflag->description = "Verbose output";

  /* Parse and process options */
  if( G_parser( argc, argv ) )
    exit(-1);

  strcpy( vecname, out->answer );
  strcpy( setname, set->answer );

  if( strcmp( setname, "" ) == 0 )
    strcpy( setname, G_mapset() );

  if( G_find_vector2( vecname, setname ) == NULL ) {
    sprintf(errbuf, "Unable to find map `%s' in mapset `%s'.\n", vecname, setname );
    G_fatal_error(errbuf);
  }

  if( proc_logfile( SET_VAL, logfile->answer ) == 0 )
    G_warning( "Cannot set specified file as log file. Using standard output.\n" );

  if( strcmp( altname->answer, "" ) == 0 ) strcpy( shapename, vecname );
  else strcpy( shapename, altname->answer );

  if( strcmp( shapetype->answer, "area") == 0 ) shptype = AREA;
  else if( strcmp( shapetype->answer, "line") == 0 ) shptype = LINE;
  else if( strcmp( shapetype->answer, "both") == 0 ) shptype = BOTH;
  else shptype = BOTH;

  strcpy( shapename_area, shapename );
  strcpy( shapename_line, shapename );

  if( shptype == BOTH ) {
    strcat( shapename_area, "_area" );
    strcat( shapename_line, "_line" );
  }
  else if( shptype == AREA )
    strcpy(shapename_area, shapename);
  else
    strcpy(shapename_line, shapename);

  if( strcmp(category->answer, "string" ) == 0 ) {
    docats = 1;
    cat_type = STR_CAT;
  }
  else if( strcmp(category->answer, "integer"  ) == 0 ) {
    docats = 1;
    cat_type = INT_CAT;
  }
  else if( strcmp(category->answer, "float"  ) == 0 ) {
    docats = 1;
    cat_type = FLT_CAT;
  }
  else docats = 0;


  /* Initialise data structures */

  /* We need to keep a running list of all the indices of areas/lines we assign to shapes */
  poly_chklist = (int *)malloc( pcl_size * sizeof(int) );
  line_chklist = (int *)malloc( pcl_size * sizeof(int) );

  /* Open shapefiles for writing */
  if( shptype == AREA || shptype == BOTH ) {
    if( (hSA = SHPCreate( shapename_area, SHPT_POLYGON )) == NULL ) {
      sprintf( errbuf, "Could not create shapefile to hold area coverage of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
  }

  if( shptype == LINE || shptype == BOTH ) {
    if( (hSL = SHPCreate( shapename_line, SHPT_ARC )) == NULL ) {
      sprintf( errbuf, "Could not create shapefile to hold line coverage of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
  }

  res = Vect_open_old( &vmap, vecname, setname );

  switch(res) {

  case 1:
    {
      sprintf( errbuf, "Vector map `%s' does not have topology. Please run v.support.\n",
	       vecname );
      G_fatal_error(errbuf);
      break;
    }
  case 2:
    break;
  default:
    {
      if(res < 0) {
	sprintf( errbuf, "Could not open vector map `%s' for processing.\n", vecname );
	G_fatal_error(errbuf);
	break;
      }
      else {
	/* Try to continue. ??? */
	break;
      }
    }
  }


  /* Extract rings */

  if( shptype == AREA || shptype == BOTH ) {
    while(found) {

      /* Keep cycling through extraction procedure till no ring is found */
      if(pcl_size < nP + 1 ) {
	pcl_size += pcl_incr;
	poly_chklist = (int *)realloc( poly_chklist, pcl_size * sizeof(int) );
      }
      found = extract_ring( &hShape, &vmap, poly_chklist, &nP, where++ );

      /* Write out the shape object */

      if(found) {
	SHPWriteObject( hSA, -1, hShape );
	SHPDestroyObject(hShape);
      }

    }
  }
  
  found = 1;
  where = 0;
  if( shptype == BOTH || shptype == LINE ) {
    while(found) {

      /* Keep cycling through extraction procedure till no ring is found */
      if(lcl_size < nL + 1 ) {
	lcl_size += lcl_incr;
	line_chklist = (int *)realloc( line_chklist, lcl_size * sizeof(int) );
      }
      found = extract_lines( &hShape, &vmap, line_chklist, &nL , where++ );      

      /* Write out the shape object */

      if(found > 0) {
	SHPWriteObject( hSL, -1, hShape );
	SHPDestroyObject(hShape);
      }
    }
  }

  if(shptype == AREA || shptype == BOTH)
    SHPClose(hSA);
  if(shptype == BOTH || shptype == LINE)
    SHPClose(hSL);


  /* Now get attributes and categories */

  fd0 = (fieldDescriptor *)malloc( 3 * sizeof(fieldDescriptor) );
  fd1 = (fieldDescriptor *)malloc( 3 * sizeof(fieldDescriptor) );

  /* Deal with the area file first */

  if( (shptype == AREA || shptype == BOTH) && nP > 0 ) {

    /* Initialize the category structure for the vector map */
    if(docats) {
      Cat1 = (struct Categories *)malloc( sizeof(struct Categories) );
      if(G_read_vector_cats( vecname, setname, Cat1 ) != 0 ) {
	docats = 0;
      }
    }

    /* Rewind the map and start extracting attributes */
    Vect_rewind(&vmap);

    /* Create record lists for all three fields */
    for( ib = 0; ib < 3; ++ib ) {
      fd0[ib].fldRecs = (dbfRecElement *)malloc( nP * sizeof(dbfRecElement) );
      fd0[ib].duff = 0;
      fd0[ib].nRec = nP;
    }
    fd0[0].fldSize = 8;
    fd0[1].fldSize = 16;
    fd0[0].fldDec = 0;
    fd0[1].fldDec = 0;
    fd0[0].fldType = FTInteger;
    fd0[1].fldType = FTInteger;
    strcpy(fd0[0].fldName, "ID");
    strcpy(fd0[1].fldName, "CAT_ID");
    strcpy(fd0[2].fldName, "CAT_VALUE");

    if(docats) { /* Only if the third field is needed */
      switch(cat_type) {

      case STR_CAT:
	{
	  fd0[2].fldSize = 128;
	  fd0[2].fldDec = 0;
	  fd0[2].fldType = FTString;
	  break;
	}
      case INT_CAT:
	{
	  fd0[2].fldSize = 10;
	  fd0[2].fldDec = 0;
	  fd0[2].fldType = FTInteger;
	  break;
	}
      case FLT_CAT:
	{
	  fd0[2].fldSize = 16;
	  fd0[2].fldDec = 6;
	  fd0[2].fldType = FTDouble;
	  break;
	}
      }
    }

    /* Run through existing indices to extract area attributes */


    for( ia = 0; ia < nP; ++ia ) {
      fd0[0].fldRecs[ia].intField = ia + 1;
      fd0[1].fldRecs[ia].intField = 0;
      
      if(docats) {
	if(fd0[2].fldType == FTString) {
	  fd0[2].fldRecs[ia].stringField = (char *)malloc(6);
	  strcpy( fd0[2].fldRecs[ia].stringField, "" );
	}

	else if(fd0[2].fldType == FTInteger) fd0[2].fldRecs[ia].intField = 0;
	else if(fd0[2].fldType == FTDouble) fd0[2].fldRecs[ia].doubleField = 0.0;
      }
    }

    for( ia = 0; ia < nP; ia++ ) {

      char *cat1;

      if( (fd0[1].fldRecs[ia].intField = V2_area_att( &vmap, poly_chklist[ia] )) == 0 )
	continue;;

      if(docats) {

	cat1 = G_get_cat( fd0[1].fldRecs[ia].intField, Cat1 );

	switch(cat_type) {
	case STR_CAT:
	  {
	    fd0[2].fldRecs[ia].stringField = (char *)realloc(fd0[2].fldRecs[ia].stringField,
										 strlen(cat1) + 1 );
	    strcpy(fd0[2].fldRecs[ia].stringField, cat1 );
	    break;
	  }
	case INT_CAT:
	  {
	    fd0[2].fldRecs[ia].intField = atoi(cat1);
	    break;
	  }
	case FLT_CAT:
	  {
	    fd0[2].fldRecs[ia].doubleField = atof(cat1);
	    break;
	  }
	}
      }

    }
    
    if( docats )
      free(Cat1);
  }

  /* Now the lines */

  if( (shptype == LINE || shptype == BOTH) && nL > 0 ) {

    /* Initialize the category structure for the vector map */
    if(docats) {
      Cat1 = (struct Categories *)malloc( sizeof(struct Categories) );
      if(G_read_vector_cats( vecname, setname, Cat1 ) != 0 ) {
	docats = 0;
      }
    }

    /* Rewind the map and start extracting attributes */
    Vect_rewind(&vmap);

    /* Create record lists for all three fields */

    for( ib = 0; ib < 3; ++ib ) {
      fd1[ib].fldRecs = (dbfRecElement *)malloc( nL * sizeof(dbfRecElement) );
      fd1[ib].duff = 0;
      fd1[ib].nRec = nL;
    }
    fd1[0].fldSize = 8;
    fd1[1].fldSize = 16;
    fd1[0].fldDec = 0;
    fd1[1].fldDec = 0;
    fd1[0].fldType = FTInteger;
    fd1[1].fldType = FTInteger;
    strcpy(fd1[0].fldName, "ID");
    strcpy(fd1[1].fldName, "CAT_ID");
    strcpy(fd1[2].fldName, "CAT_VALUE");

    if(docats) { /* Only if the third field is required */
      switch(cat_type) {

      case STR_CAT:
	{
	  fd1[2].fldSize = 128;
	  fd1[2].fldDec = 0;
	  fd1[2].fldType = FTString;
	  break;
	}
      case INT_CAT:
	{
	  fd1[2].fldSize = 10;
	  fd1[2].fldDec = 0;
	  fd1[2].fldType = FTInteger;
	  break;
	}
      case FLT_CAT:
	{
	  fd1[2].fldSize = 16;
	  fd1[2].fldDec = 6;
	  fd1[2].fldType = FTDouble;
	  break;
	}
      }
    }

    /* Run through existing indices to extract line attributes */

    for( ia = 0; ia < nL; ++ia ) {
      
      fd1[0].fldRecs[ia].intField = ia + 1;
      fd1[1].fldRecs[ia].intField = 0;

      if(docats) {
	if(fd1[2].fldType == FTString) {
	  fd1[2].fldRecs[ia].stringField = (char *)malloc(6);
	  strcpy( fd1[2].fldRecs[ia].stringField, "" );
	}

	else if(fd1[2].fldType == FTInteger) fd0[2].fldRecs[ia].intField = 0;
	else if(fd1[2].fldType == FTDouble) fd0[2].fldRecs[ia].doubleField = 0.0;
      }

    }

    for( ia = 0; ia < nL; ia++ ) {

      char *cat1;

      if( (fd1[1].fldRecs[ia].intField = V2_line_att( &vmap, line_chklist[ia] )) == 0 )
	continue;;

      if(docats) {

	cat1 = G_get_cat( fd1[1].fldRecs[ia].intField, Cat1 );

	switch(cat_type) {
	case STR_CAT:
	  {
	    fd1[2].fldRecs[ia].stringField = (char *)realloc(fd1[2].fldRecs[ia].stringField,
										 strlen(cat1) + 1 );
	    strcpy(fd1[2].fldRecs[ia].stringField, cat1 );
	    break;
	  }
	case INT_CAT:
	  {
	    fd1[2].fldRecs[ia].intField = atoi(cat1);
	    break;
	  }
	case FLT_CAT:
	  {
	    fd1[2].fldRecs[ia].doubleField = atof(cat1);
	    break;
	  }
	}
      }

    }
    
    if( docats )
      free(Cat1);
  }

  /* Add field descriptor contents to DBF Files */

  if( (shptype == AREA || shptype == BOTH) && nP > 0 ) {
    if( (db0 = DBFCreate( shapename_area )) == NULL ) {
      sprintf( errbuf, "Could not create database file to hold area attributes of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
    if( DBFAddField(db0, "ID", FTInteger, 10, 0) < 0 ) {
      sprintf( errbuf, "Unable to create DBF field `ID'\n" );
      G_fatal_error(errbuf);
    }
    if( DBFAddField(db0, "CAT_ID", FTInteger, 10, 0) < 0 ) {
      sprintf( errbuf, "Unable to create DBF field `CAT_ID'\n" );
      G_fatal_error(errbuf);
    }

    if(docats) {
      switch(cat_type) {

      case STR_CAT:
	{
	  if( DBFAddField(db0, "CAT_DESC", FTString, 64, 0) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      case INT_CAT:
	{
	  if( DBFAddField(db0, "CAT_DESC", FTInteger, 10, 0) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      case FLT_CAT:
	{
	  if( DBFAddField(db0, "CAT_DESC", FTDouble, 16, 6) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      }
    }

  }

  if( (shptype == LINE || shptype == BOTH) && nL > 0 ) {
    if( (db1 = DBFCreate( shapename_line )) == NULL ) {
      sprintf( errbuf, "Could not create database to hold line attributes of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
    if( DBFAddField(db1, "ID", FTInteger, 10, 0) < 0 ) {
      sprintf( errbuf, "Unable to create DBF field `ID'\n" );
      G_fatal_error(errbuf);
    }
    if( DBFAddField(db1, "CAT_ID", FTInteger, 10, 0) < 0 ) {
      sprintf( errbuf, "Unable to create DBF field `CAT_ID'\n" );
      G_fatal_error(errbuf);
    }

    if(docats) {
      switch(cat_type) {

      case STR_CAT:
	{
	  if( DBFAddField(db1, "CAT_DESC", FTString, 64, 0) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      case INT_CAT:
	{
	  if( DBFAddField(db1, "CAT_DESC", FTInteger, 10, 0) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      case FLT_CAT:
	{
	  if( DBFAddField(db1, "CAT_DESC", FTDouble, 16, 6) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      }
    }
  }

  if(docats) no = 3;
  else no = 2;

  if( (shptype == AREA || shptype == BOTH) && nP > 0 ) {
    if( load_dbf_from_fieldD( db0, fd0, no ) != 0 ) {
      sprintf( errbuf, "Could not create dbf file for areas file `%s'.\n", vecname );
      G_fatal_error(errbuf);
    }
    DBFClose(db0);
  }

  if( (shptype == LINE || shptype == BOTH) && nL > 0 ) {
    if( load_dbf_from_fieldD( db1, fd1, no ) != 0 ) {
      sprintf( errbuf, "Could not create dbf file for lines file `%s'.\n", vecname );
      G_fatal_error(errbuf);
    }
    DBFClose(db1);
  }

  Vect_close(&vmap);

  if( shptype == BOTH && nL == 0 ) {
    strcat( strcpy(delfile, shapename_line), ".shp" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
    strcat( strcpy(delfile, shapename_line), ".shx" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
  };

  if( shptype == BOTH && nP == 0 ) {
    strcat( strcpy(delfile, shapename_area), ".shp" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
    strcat( strcpy(delfile, shapename_area), ".shx" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
  };

  return 0;
}


