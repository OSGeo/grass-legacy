/******************************************************************************
 * main.c [v.out.shape2]
 * export vector map as ESRI shapefile.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 29th. Jun. 2000
 * Last updated 2nd. Jul. 2000
 *
 * DBMI support by RadimBlazek 12/2001
 
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
  struct Option *table, *key;
  struct Flag *chatflag, *Linetype, *Areatype;
  struct Map_info vmap;
  struct Categories *Cat1;
  fieldDescriptor *fd[3];
  DBFHandle db0, db1, db2;
  char vecname[128], setname[128], shapename[128];
  char shapename_area[128], shapename_line[128], shapename_point[128], delfile[128];
  char errbuf[512];
  int docats, cat_type = 0;
  int dotable = 0;
  int shptype,linetype;
  int rec_size = 1000, rec_incr = 500;

  /* Data processing structures */
  int *poly_chklist, *line_chklist, *point_chklist, *rev_index;
  int pcl_size = 1000, pcl_incr = 500;
  int lcl_size = 1000, lcl_incr = 500;
  int dcl_size = 1000, dcl_incr = 500;
  SHPHandle hSA, hSL, hSP;
  SHPObject *hShape;
  int found = 1, where = 0;
  int nP = 0, nL = 0, nD = 0, n ;
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
  shapetype->multiple = YES;
  shapetype->description = "Type of coverage (area, line or point)";
  shapetype->options = "area,line,point";
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

  table  = G_define_option();
  table->key = "table";
  table->type = TYPE_STRING;
  table->required = NO;
  table->description = "Database table to read data from";
  table->answer  = "";

  key  = G_define_option();
  key->key = "key";
  key->type = TYPE_STRING;
  key->required = NO;
  key->description = "Key column";
  key->answer  = "";

  /* Set flags */

  chatflag = G_define_flag();
  chatflag->key = 'v';
  chatflag->description = "Verbose output";

  Linetype = G_define_flag();
  Linetype->key = 'l';
  Linetype->description = "Restrict lines to arcs of type line";

  Areatype = G_define_flag();
  Areatype->key = 'a';
  Areatype->description = "Restrict lines to arcs of type area edge";

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

  ia = 0;
  shptype = 0; 
  while (shapetype->answers[ia]) {
      if( strcmp( shapetype->answers[ia], "area") == 0 ) shptype |= AREA;
      else if( strcmp( shapetype->answers[ia], "line") == 0 ) shptype |= LINE;
      else if( strcmp( shapetype->answers[ia], "point") == 0 ) shptype |= DOT;
      ia++;
  }

  strcpy( shapename_area, shapename );
  strcpy( shapename_line, shapename );
  strcpy( shapename_point, shapename );

  if( ia > 1 ) {
    strcat( shapename_area, "_area" );
    strcat( shapename_line, "_line" );
    strcat( shapename_point, "_point" );
  }

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

  linetype=3;
  if(shptype == LINE)
  {
	if(Linetype->answer)linetype=LINE;
	if(Areatype->answer)linetype=AREA;
  }

  if( strcmp( table->answer, "" ) != 0 ) dotable = 1;

  /* Initialise data structures */

  /* We need to keep a running list of all the indices of areas/lines we assign to shapes */
  poly_chklist = (int *)malloc( pcl_size * sizeof(int) );
  line_chklist = (int *)malloc( lcl_size * sizeof(int) );
  point_chklist = (int *)malloc( dcl_size * sizeof(int) );

  /* Open shapefiles for writing */
  if( shptype & AREA ) {
    if( (hSA = SHPCreate( shapename_area, SHPT_POLYGON )) == NULL ) {
      sprintf( errbuf, "Could not create shapefile '%s' to hold area coverage of map `%s'.\n",
	      shapename_area, vecname );
      G_fatal_error(errbuf);
    }
  }

  if( shptype & LINE ) {
    if( (hSL = SHPCreate( shapename_line, SHPT_ARC )) == NULL ) {
      sprintf( errbuf, "Could not create shapefile '%s' to hold line coverage of map `%s'.\n",
	       shapename_line, vecname );
      G_fatal_error(errbuf);
    }
  }
  
  if( shptype & DOT ) {
    if( (hSP = SHPCreate( shapename_point, SHPT_POINT )) == NULL ) {
      sprintf( errbuf, "Could not create shapefile '%s' to hold point coverage of map `%s'.\n",
	       shapename_point, vecname );
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

  if( shptype & AREA ) {
    while(found) {

      /* Keep cycling through extraction procedure till no ring is found */
      if(pcl_size < nP + 1 ) {
	pcl_size += pcl_incr;
	poly_chklist = (int *)realloc( poly_chklist, pcl_size * sizeof(int) );
      }
      found = extract_ring( &hShape, &vmap, poly_chklist, &nP, where++ );
      
      /* go to next area not extracted (may be used for skipping not 
         labeled islands in future) */
      if (found == -1) { found = 1; continue; }

      /* Write out the shape object */

      if(found) {
	SHPWriteObject( hSA, -1, hShape );
	SHPDestroyObject(hShape);
      } 

    }
  }
  
  found = 1;
  where = 0;
  if( shptype & LINE ) {
    while(found) {

      /* Keep cycling through extraction procedure till no ring is found */
      if(lcl_size < nL + 1 ) {
	lcl_size += lcl_incr;
	line_chklist = (int *)realloc( line_chklist, lcl_size * sizeof(int) );
      }
      found = extract_lines( &hShape, &vmap, line_chklist, &nL , where++ ,linetype );      
      
      /* go to next line if line is not LINE */
      if (found == -1) { found = 1; continue; }

      /* Write out the shape object */

      if(found > 0) {
	SHPWriteObject( hSL, -1, hShape );
	SHPDestroyObject(hShape);
      }
    }
  }
  
  found = 1;
  where = 0;
  if( shptype & DOT ) {
    while(found) {
      /* Keep cycling through extraction procedure till no point is found */
      if(dcl_size < nD + 1 ) {
	dcl_size += dcl_incr;
	point_chklist = (int *)realloc( point_chklist, dcl_size * sizeof(int) );
      }
      found = extract_points( &hShape, &vmap, point_chklist, &nD , where++ );      
      
      /* go to next line if line is not DOT */
      if (found == -1) { found = 1; continue; }

      /* Write out the shape object */
      if(found > 0) {
	SHPWriteObject( hSP, -1, hShape );
	SHPDestroyObject(hShape);
      }
    }
  }

  if(shptype & AREA)
    SHPClose(hSA);
  if(shptype & LINE)
    SHPClose(hSL);
  if(shptype & DOT)
    SHPClose(hSP);


  /* Now get attributes and categories */


  /* ia: 0=area, 1=line, 2=point */
  for ( ia = 0; ia < 3; ia++) {
    fd[ia] = (fieldDescriptor *)malloc( 3 * sizeof(fieldDescriptor) );
    if( ia == 0 && (shptype & AREA) && nP > 0 ) {
        n = nP;
    } else if( ia == 1 && (shptype & LINE) && nL > 0 ) {
        n = nL;
    } else if( ia == 2 && (shptype & DOT) && nD > 0 ) {
        n = nD;
    } else { continue; }
	    
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
      fd[ia][ib].fldRecs = (dbfRecElement *)malloc( n * sizeof(dbfRecElement) );
      fd[ia][ib].duff = 0;
      fd[ia][ib].nRec = n;
    }
    fd[ia][0].fldSize = 8;
    fd[ia][1].fldSize = 16;
    fd[ia][0].fldDec = 0;
    fd[ia][1].fldDec = 0;
    fd[ia][0].fldType = FTInteger;
    fd[ia][1].fldType = FTInteger;
    strcpy(fd[ia][0].fldName, "ID");
    strcpy(fd[ia][1].fldName, "CAT_ID");
    strcpy(fd[ia][2].fldName, "CAT_VALUE");

    if(docats) { /* Only if the third field is needed */
      switch(cat_type) {

      case STR_CAT:
	{
	  fd[ia][2].fldSize = 128;
	  fd[ia][2].fldDec = 0;
	  fd[ia][2].fldType = FTString;
	  break;
	}
      case INT_CAT:
	{
	  fd[ia][2].fldSize = 10;
	  fd[ia][2].fldDec = 0;
	  fd[ia][2].fldType = FTInteger;
	  break;
	}
      case FLT_CAT:
	{
	  fd[ia][2].fldSize = 16;
	  fd[ia][2].fldDec = 6;
	  fd[ia][2].fldType = FTDouble;
	  break;
	}
      }
    }

    /* Run through existing indices to extract area attributes */

    for( ib = 0; ib < n; ++ib ) {
      fd[ia][0].fldRecs[ib].intField = ib + 1;
      fd[ia][1].fldRecs[ib].intField = 0;
      
      if(docats) {
	if(fd[ia][2].fldType == FTString) {
	  fd[ia][2].fldRecs[ib].stringField = (char *)malloc(6);
	  strcpy( fd[ia][2].fldRecs[ib].stringField, "" );
	}

	else if(fd[ia][2].fldType == FTInteger) fd[ia][2].fldRecs[ib].intField = 0;
	else if(fd[ia][2].fldType == FTDouble) fd[ia][2].fldRecs[ib].doubleField = 0.0;
      }
    }

    for( ib = 0; ib < n; ib++ ) {

      char *cat1;

      if (ia == 0)
          if( (fd[ia][1].fldRecs[ib].intField = V2_area_att( &vmap, poly_chklist[ib] )) == 0 )
            continue;;
      
      if (ia == 1)
          if( (fd[ia][1].fldRecs[ib].intField = V2_line_att( &vmap, line_chklist[ib] )) == 0 )
            continue;;
      
      if (ia == 2)
          if( (fd[ia][1].fldRecs[ib].intField = vmap.Att[vmap.Line[point_chklist[ib]].att].cat ) == 0 )
            continue;;

      
      if(docats) {

	cat1 = G_get_cat( fd[ia][1].fldRecs[ib].intField, Cat1 );

	switch(cat_type) {
	case STR_CAT:
	  {
	    fd[ia][2].fldRecs[ib].stringField = (char *)realloc(fd[ia][2].fldRecs[ib].stringField,
										 strlen(cat1) + 1 );
	    strcpy(fd[ia][2].fldRecs[ib].stringField, cat1 );
	    break;
	  }
	case INT_CAT:
	  {
	    fd[ia][2].fldRecs[ib].intField = atoi(cat1);
	    break;
	  }
	case FLT_CAT:
	  {
	    fd[ia][2].fldRecs[ib].doubleField = atof(cat1);
	    break;
	  }
	}
      }

    }
    
    if( docats )
      free(Cat1);
  }


  /* Add field descriptor contents to DBF Files */

  if( (shptype & AREA) && nP > 0 ) {
    if( (db0 = DBFCreate( shapename_area )) == NULL ) {
      sprintf( errbuf, "Could not create database file to hold area attributes of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
    if (!dotable) {
        if( DBFAddField(db0, "ID", FTInteger, 10, 0) < 0 ) {
          sprintf( errbuf, "Unable to create DBF field `ID'\n" );
          G_fatal_error(errbuf);
        }
        if( DBFAddField(db0, "CAT_ID", FTInteger, 10, 0) < 0 ) {
          sprintf( errbuf, "Unable to create DBF field `CAT_ID'\n" );
          G_fatal_error(errbuf);
        }
    }
	
    if(docats && !dotable) {
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

  if( (shptype & LINE) && nL > 0 ) {
    if( (db1 = DBFCreate( shapename_line )) == NULL ) {
      sprintf( errbuf, "Could not create database to hold line attributes of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
    if (!dotable) { 
        if( DBFAddField(db1, "ID", FTInteger, 10, 0) < 0 ) {
          sprintf( errbuf, "Unable to create DBF field `ID'\n" );
          G_fatal_error(errbuf);
        }
        if( DBFAddField(db1, "CAT_ID", FTInteger, 10, 0) < 0 ) {
          sprintf( errbuf, "Unable to create DBF field `CAT_ID'\n" );
          G_fatal_error(errbuf);
        }
    }

    if(docats && !dotable) {
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
  
  if( (shptype & DOT) && nD > 0 ) {
    if( (db2 = DBFCreate( shapename_point )) == NULL ) {
      sprintf( errbuf, "Could not create database to hold point attributes of map `%s'.\n",
	       vecname );
      G_fatal_error(errbuf);
    }
    if (!dotable) { 
        if( DBFAddField(db2, "ID", FTInteger, 10, 0) < 0 ) {
          sprintf( errbuf, "Unable to create DBF field `ID'\n" );
          G_fatal_error(errbuf);
        }
        if( DBFAddField(db2, "CAT_ID", FTInteger, 10, 0) < 0 ) {
          sprintf( errbuf, "Unable to create DBF field `CAT_ID'\n" );
          G_fatal_error(errbuf);
        }
    }

    if(docats && !dotable) {
      switch(cat_type) {

      case STR_CAT:
	{
	  if( DBFAddField(db2, "CAT_DESC", FTString, 64, 0) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      case INT_CAT:
	{
	  if( DBFAddField(db2, "CAT_DESC", FTInteger, 10, 0) < 0 ) {
	    sprintf( errbuf, "Unable to create DBF field `CAT_DESC'\n" );
	    G_fatal_error(errbuf);
	  }
	  break;
	}
      case FLT_CAT:
	{
	  if( DBFAddField(db2, "CAT_DESC", FTDouble, 16, 6) < 0 ) {
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

  if( (shptype & AREA) && nP > 0 ) {
    if (!dotable) 
        res =  load_dbf_from_fieldD( db0, fd[0], no );
    else
        res =  load_dbf_from_table( db0, fd[0], table->answer, key->answer );
		
    if( res  != 0 ) {
      sprintf( errbuf, "Could not create dbf file for areas file `%s'.\n", vecname );
      G_fatal_error(errbuf);
    }
    DBFClose(db0);
  }

  if( (shptype & LINE) && nL > 0 ) {
    if (!dotable) 
        res =  load_dbf_from_fieldD( db1, fd[1], no );
    else
        res =  load_dbf_from_table( db1, fd[1], table->answer, key->answer );
		
    if( res  != 0 ) {
      sprintf( errbuf, "Could not create dbf file for lines file `%s'.\n", vecname );
      G_fatal_error(errbuf);
    }
    DBFClose(db1);
  }
  
  if( (shptype & DOT) && nD > 0 ) {
    if (!dotable) 
        res =  load_dbf_from_fieldD( db2, fd[2], no );
    else
        res =  load_dbf_from_table( db2, fd[2], table->answer, key->answer );
		
    if( res  != 0 ) {
      sprintf( errbuf, "Could not create dbf file for points file `%s'.\n", vecname );
      G_fatal_error(errbuf);
    }
    DBFClose(db2);
  }

  Vect_close(&vmap);

  if( shptype & LINE && nL == 0 ) {
    strcat( strcpy(delfile, shapename_line), ".shp" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
    strcat( strcpy(delfile, shapename_line), ".shx" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
  };

  if( shptype & AREA && nP == 0 ) {
    strcat( strcpy(delfile, shapename_area), ".shp" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
    strcat( strcpy(delfile, shapename_area), ".shx" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
  };
  
  if( shptype & DOT && nD == 0 ) {
    strcat( strcpy(delfile, shapename_point), ".shp" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
    strcat( strcpy(delfile, shapename_point), ".shx" );
    if(unlink(delfile) < 0 )
      G_warning( "Could not delete file %s\n", delfile );
  };

  
  return 0;
}

