/* $Id$
 * ***************************************************************
 * *
 * * MODULE:       v.out.ogr
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               Some extensions: Markus Neteler
 * *
 * * PURPOSE:      Category manipulations
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h> 
#include <string.h> 
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "ogr_api.h"
#include "cpl_string.h"
#include "config.h"
#include "gprojects.h"

int    fout, fskip; /* features written/ skip */
int    nocat, noatt, nocatskip; /* number of features without cats/atts written/skip */
	
int mk_att ( int cat, struct field_info *Fi, dbDriver *Driver, int ncol, int keycol, int doatt, 
	 OGRFeatureH Ogr_feature);

char *OGR_list_write_drivers();
char OGRdrivers[2000];

int 
main (int argc, char *argv[])
{
    int    i, j, k, centroid, otype, wkbtype=wkbUnknown, donocat;
    char   *mapset;
    int    field;
    struct GModule *module;
    struct Option *in_opt, *dsn_opt, *layer_opt, *type_opt, *frmt_opt, *field_opt, *dsco, *lco;
    struct Flag   *cat_flag;
    char   buf[2000], *pbuf;
    char   key1[200], key2[200];
    struct Key_Value *projinfo, *projunits;
    struct Cell_head cellhd;

    /* Vector */
    struct Map_info In;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int    type, cat;

    /* Attributes */
    int doatt=0, ncol=0, colsqltype, colctype, keycol=-1;
    struct field_info *Fi=NULL;
    dbDriver *Driver=NULL;
    dbHandle handle;
    dbTable *Table;
    dbString dbstring;
    dbColumn *Column;
    
    /* OGR */
    int drn, ogr_ftype=OFTInteger;
    OGRDataSourceH Ogr_ds;
    OGRSFDriverH Ogr_driver;  
    OGRLayerH Ogr_layer;	
    OGRFieldDefnH Ogr_field; 
    OGRFeatureH Ogr_feature;  
    OGRFeatureDefnH Ogr_featuredefn;
    OGRGeometryH Ogr_geometry;
    OGRSpatialReferenceH Ogr_projection;
    char **papszDSCO = NULL, **papszLCO = NULL;

    G_gisinit(argv[0]);

    /* Module options */
    module = G_define_module();
    module->description = "Convert to OGR format.";

    in_opt = G_define_standard_option(G_OPT_V_INPUT);

    type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
    type_opt->answer = "line,boundary";
    type_opt->description = "Feature type. Possible combinations of more types: "
			    "point,centroid or line,boundary.";
    
    dsn_opt = G_define_option();
    dsn_opt->key = "dsn";
    dsn_opt->type =  TYPE_STRING;
    dsn_opt->required = YES;
    dsn_opt->description = "OGR datasource name.\n"
			   "\t\tESRI Shapefile: directory containing shapefiles\n"
			   "\t\tMapInfo File: directory containing mapinfo files";

    layer_opt = G_define_option();
    layer_opt->key = "layer";
    layer_opt->type = TYPE_STRING;
    layer_opt->required = YES;
    layer_opt->description = "OGR layer name.\n"
			   "\t\tESRI Shapefile: shapefile name\n"
			   "\t\tMapInfo File: mapinfo file name";
    
    field_opt = G_define_standard_option(G_OPT_V_FIELD);
    field_opt->answer = "1";

    frmt_opt = G_define_option();
    frmt_opt->key = "format";
    frmt_opt->type =  TYPE_STRING;
    frmt_opt->required = NO;
    frmt_opt->multiple = NO;
    frmt_opt->answer = "ESRI_Shapefile";
    frmt_opt->options = OGR_list_write_drivers();
    frmt_opt->description = "OGR format.";
    
    dsco              = G_define_option();
    dsco->key         = "dsco";
    dsco->type        = TYPE_STRING;
    dsco->required    = NO;
    dsco->multiple    = NO;
    dsco->answer      = "";
    dsco->description = "OGR dataset creation option (format specific, NAME=VALUE)";
    
    lco               = G_define_option();
    lco->key          = "lco";
    lco->type         = TYPE_STRING;
    lco->required     = NO;
    lco->multiple     = NO;
    lco->answer       = "";
    lco->description  = "OGR layer creation option (format specific, NAME=VALUE)";
    
    cat_flag = G_define_flag ();
    cat_flag->key            = 'c';
    cat_flag->description    = "Export features with category (labeled) only. "
			       "Otherwise all features are exported";

    if (G_parser (argc, argv)) exit(1); 
    
    /* read options */
    field = atoi( field_opt->answer );
    
    /* Check output type */
    otype = Vect_option_to_types ( type_opt ); 
    if ( ((GV_POINTS & otype) && (GV_LINES & otype)) ||
	 ((GV_POINTS & otype) && (GV_AREA & otype)) || 
	 ((GV_LINES & otype) && (GV_AREA & otype)) ) 
      {
	 G_fatal_error ("The combination of types is not allowed.");
      }

    if ( otype & GV_POINTS ) wkbtype = wkbPoint;
    else if ( otype & GV_LINES ) wkbtype = wkbLineString;
    else if ( otype & GV_AREA ) wkbtype = wkbPolygon;
    
    if ( cat_flag->answer ) donocat = 0; else donocat = 1;
    
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    /* open input vector */
    if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
	 G_fatal_error ("Could not find input map <%s>\n", in_opt->answer);
    }
    
    Vect_set_open_level (2); 
    Vect_open_old (&In, in_opt->answer, mapset); 

    /* fetch PROJ info */
    G_get_default_window(&cellhd);
    if( cellhd.proj == PROJECTION_XY )
        Ogr_projection = NULL;
    else
    {
        projinfo = G_get_projinfo();
        projunits = G_get_projunits();
        Ogr_projection = GPJ_grass_to_osr(projinfo, projunits);
    }

    /* Open OGR DSN */
    OGRRegisterAll();
    G_debug (2, "driver count = %d", OGRGetDriverCount() ); 
    drn = -1;
    for ( i = 0; i < OGRGetDriverCount(); i++ ) {
	Ogr_driver = OGRGetDriver( i );  
	G_debug (2, "driver %d : %s", i, OGR_Dr_GetName ( Ogr_driver) ); 
        /* chg white space to underscore in OGR driver names */
	sprintf (buf, "%s", OGR_Dr_GetName ( Ogr_driver) );
	G_strchg(buf,' ','_');
	if ( strcmp ( buf ,  frmt_opt->answer ) == 0 ) {
	    drn = i;
	    G_debug (2, " -> driver = %d" ); 
	}
    }
    if ( drn == -1 ) G_fatal_error ( "Driver %s not found", frmt_opt->answer ); 
    Ogr_driver = OGRGetDriver( drn );
    papszDSCO = CSLSetNameValue( papszDSCO, dsco->answer,"YES");
    Ogr_ds = OGR_Dr_CreateDataSource( Ogr_driver, dsn_opt->answer, papszDSCO );
    CSLDestroy( papszDSCO );
    if ( Ogr_ds == NULL ) G_fatal_error ("Cannot open OGR data source '%s'", dsn_opt->answer);
    
    papszLCO = CSLSetNameValue( papszLCO, lco->answer,"YES");
    Ogr_layer = OGR_DS_CreateLayer( Ogr_ds, layer_opt->answer, Ogr_projection, wkbtype, papszLCO );
    CSLDestroy( papszLCO );
    if ( Ogr_layer == NULL ) G_fatal_error ("Cannot create layer");
    
    db_init_string(&dbstring);

    /* Vector attributes -> OGR fields */
    if ( field > 0 ) {
	 doatt = 1; /* do attributes */
	 Fi = Vect_get_field( &In, field);
	 if ( Fi == NULL ) {
	     G_warning ("Cannot read field info -> using categories only for attributes");
	     
	     Ogr_field = OGR_Fld_Create( "cat", OFTInteger ); 
	     OGR_L_CreateField( Ogr_layer, Ogr_field, 0 ); 
	     
	     doatt = 0;
	 } else {  
	     Driver = db_start_driver(Fi->driver);
	     if (Driver == NULL) G_fatal_error("Cannot open driver %s", Fi->driver);

	     db_init_handle (&handle);
	     db_set_handle (&handle, Fi->database, NULL);
	     if (db_open_database(Driver, &handle) != DB_OK)
		 G_fatal_error("Cannot open database %s", Fi->database);

	     db_set_string(&dbstring, Fi->table);
	     if(db_describe_table (Driver, &dbstring, &Table) != DB_OK) 
		 G_fatal_error("Cannot describe table %s", Fi->table);

	     ncol = db_get_table_number_of_columns(Table); 
	     G_debug (2, "ncol = %d", ncol );
	     keycol = -1;
	     for (i = 0; i < ncol; i++) {
		 Column = db_get_table_column (Table, i);
		 colsqltype = db_get_column_sqltype(Column);
		 G_debug (2, "col %d: %s (%s)", i, db_get_column_name(Column),
			   db_sqltype_name(colsqltype) );
		 colctype = db_sqltype_to_Ctype ( colsqltype );

		 switch ( colctype ) {
		     case DB_C_TYPE_INT:
			ogr_ftype = OFTInteger;
			break; 
		     case DB_C_TYPE_DOUBLE:
			ogr_ftype = OFTReal;
			break; 
		     case DB_C_TYPE_STRING:
			ogr_ftype = OFTString;
			break; 
		     case DB_C_TYPE_DATETIME:
			ogr_ftype = OFTString;
			break; 
		 }
		 G_debug (2, "ogr_ftype = %d", ogr_ftype );

		 strcpy ( key1, Fi->key ); G_tolcase ( key1 );
		 strcpy ( key2, db_get_column_name(Column) ); G_tolcase ( key2 );
		 if ( strcmp(key1, key2) == 0 ) keycol = i;
		 G_debug (2, "%s x %s -> %s x %s -> keycol = %d", Fi->key, 
			 db_get_column_name(Column), key1, key2, keycol );
		 
		 Ogr_field = OGR_Fld_Create( db_get_column_name(Column), ogr_ftype ); 
		 OGR_L_CreateField( Ogr_layer, Ogr_field, 0 ); 
	     }
	     if ( keycol == -1 ) G_fatal_error ("Key column '%s' not found", Fi->key );
	 }
	 
    }

    Ogr_featuredefn = OGR_L_GetLayerDefn( Ogr_layer );
    Ogr_feature = OGR_F_Create( Ogr_featuredefn );

    fout = fskip = nocat = noatt = nocatskip = 0;

    /* check what users wants to export and what's present in the map */
    if ( Vect_get_num_primitives(&In, GV_POINT) > 0 && !(otype & GV_POINTS) )
      G_warning("%d Point(s) found, but not requested to be exported. Verify 'type' parameter.",Vect_get_num_primitives(&In, GV_POINT));

    if ( Vect_get_num_primitives(&In, GV_LINE) > 0 && !(otype & GV_LINES) )
      G_warning("%d Line(s) found, but not requested to be exported. Verify 'type' parameter.",Vect_get_num_primitives(&In, GV_LINE));

    if ( Vect_get_num_primitives(&In, GV_BOUNDARY) > 0 && !(otype & GV_BOUNDARY) && !(otype & GV_AREA) )
      G_warning("%d Boundary(ies) found, but not requested to be exported. Verify 'type' parameter.",Vect_get_num_primitives(&In, GV_BOUNDARY));

    if ( Vect_get_num_primitives(&In, GV_CENTROID) > 0 && !(otype & GV_CENTROID) && !(otype & GV_AREA) )
      G_warning("%d Centroid(s) found, but not requested to be exported. Verify 'type' parameter.",Vect_get_num_primitives(&In, GV_CENTROID));

    if ( Vect_get_num_primitives(&In, GV_AREA) > 0  && !(otype & GV_AREA) )
       G_warning("%d Areas found, but not requested to be exported. Verify 'type' parameter.",Vect_get_num_primitives(&In, GV_AREA));

    /* add? GV_FACE GV_KERNEL */

    /* Lines (run always to count features of different type) */
    if ( (otype & GV_POINTS) || (otype & GV_LINES) ) {
	fprintf(stderr,"Exporting %i points/lines...\n", Vect_get_num_lines(&In) );
	for ( i = 1; i <= Vect_get_num_lines(&In) ; i++ ) {

	    G_percent(i,Vect_get_num_lines(&In),1);

	    type = Vect_read_line (&In, Points, Cats, i);
	    G_debug (2, "line = %d type = %d", i, type );
	    if ( !(otype & type ) ) {
		 G_debug (2, "type %d not specified -> skip", type );
		 fskip++;
		 continue;
	    }

	    Vect_cat_get (Cats, field, &cat);
	    if ( cat < 0 && !donocat ) { /* Do not export not labeled */ 
		nocatskip++;
		continue; 
	    }


	    /* Geometry */
	    Ogr_geometry = OGR_G_CreateGeometry( wkbtype );
	    for ( j = 0; j < Points->n_points; j++ ) {
		OGR_G_AddPoint( Ogr_geometry, Points->x[j], Points->y[j], Points->z[j] );
	    }
	    OGR_F_SetGeometry( Ogr_feature, Ogr_geometry ); 

	    /* Output one feature for each category */
	    for ( j = -1; j < Cats->n_cats; j++ ) {
		if ( j == -1 ) {
		    if ( cat >= 0 ) continue; /* cat(s) exists */
		} else {
		    if ( Cats->field[j] == field )
			cat = Cats->cat[j];
		    else 
			continue;
		}
	        
		mk_att ( cat, Fi, Driver, ncol, keycol, doatt, Ogr_feature);
	        OGR_L_CreateFeature( Ogr_layer, Ogr_feature ); 
	    }
	    OGR_G_DestroyGeometry( Ogr_geometry );
	}
    }

    /* Areas (run always to count features of different type) */
    if ( otype & GV_AREA ) {
	fprintf(stderr,"Exporting %i areas (may take some time) ...\n", Vect_get_num_areas(&In) );
	for ( i = 1; i <= Vect_get_num_areas(&In) ; i++ ) {
	    OGRGeometryH    ring;
	    
	    G_percent(i,Vect_get_num_areas(&In),1);
	    
	    centroid = Vect_get_area_centroid ( &In, i );
	    cat = -1;
	    if ( centroid > 0 ) {
		Vect_read_line (&In, NULL, Cats, centroid );
		Vect_cat_get (Cats, field, &cat);
	    }
	    G_debug (3, "area = %d centroid = %d ncats = %d", i, centroid, Cats->n_cats );
	    if ( cat < 0 && !donocat ) { /* Do not export not labeled */ 
		nocatskip++;
		continue; 
	    }

	    Vect_get_area_points ( &In, i, Points );

	    /* Geometry */
	    /* TODO: Use something better than WKT */
	    Ogr_geometry = OGR_G_CreateGeometry( wkbtype ); /* wkbPolygon */
	    
	
	    ring = OGR_G_CreateGeometry( wkbLinearRing );
	    
	    /* Area */
	    for ( j = 0; j < Points->n_points; j++ ) {
		OGR_G_AddPoint( ring, Points->x[j], Points->y[j], Points->z[j] );
	    }
	    
	    OGR_G_AddGeometryDirectly ( Ogr_geometry, ring );

	    /* Isles */
	    for ( k = 0; k < Vect_get_area_num_isles (&In, i); k++ ) {
		Vect_get_isle_points ( &In, Vect_get_area_isle (&In, i, k), Points );

		ring = OGR_G_CreateGeometry( wkbLinearRing );
		for ( j = 0; j < Points->n_points; j++ ) {
		    OGR_G_AddPoint( ring, Points->x[j], Points->y[j], Points->z[j] );
		}
		OGR_G_AddGeometryDirectly ( Ogr_geometry, ring );
	    }
	    
	    OGR_F_SetGeometry( Ogr_feature, Ogr_geometry ); 

	    /* Output one feature for each category */
	    for ( j = -1; j < Cats->n_cats; j++ ) {
		if ( j == -1 ) {
		    if ( cat >= 0 ) continue; /* cat(s) exists */
		} else {
		    if ( Cats->field[j] == field )
			cat = Cats->cat[j];
		    else 
			continue;
		}
	        
		mk_att ( cat, Fi, Driver, ncol, keycol, doatt, Ogr_feature);
	        OGR_L_CreateFeature( Ogr_layer, Ogr_feature ); 
	    }
	    OGR_G_DestroyGeometry( Ogr_geometry );
	}
    }
	    
    OGR_F_Destroy( Ogr_feature );
    OGR_DS_Destroy( Ogr_ds );
    
    Vect_close (&In);

    if ( doatt ) {
	db_close_database(Driver);
	db_shutdown_driver(Driver);
    }

    /* Summary */
    fprintf (stderr, "%d features written\n", fout);
    if ( nocat > 0 ) G_warning ( "%d features without category written", nocat);
    if ( noatt > 0 ) G_warning ( "%d features without attributes written", noatt);
    if ( nocatskip > 0 ) G_warning ( "%d features without category skip", nocatskip);

    /* Enable this? May be confusing that for area type are not reported all boundaries/centroids.
    *  OTOH why should be reported? */
    /*
    if ( ((otype & GV_POINTS) || (otype & GV_LINES)) && fskip > 0 ) 
	G_warning ( "%d features of different type skip", fskip);
    */
	 
    exit(0) ;
}

int
mk_att ( int cat, struct field_info *Fi, dbDriver *Driver, int ncol, int keycol, int doatt, 
	 OGRFeatureH Ogr_feature)
{
    int      j;
    char     buf[2000];
    int      colsqltype, colctype, more;
    dbTable  *Table;
    dbString dbstring;
    dbColumn *Column;
    dbCursor cursor;
    dbValue  *Value;

    G_debug (2, "mk_att() cat = %d, doatt = %d", cat, doatt );
    db_init_string(&dbstring);
    
    /* Attributes */
    /* Reset */
    if ( doatt ) {
	for (j = 0; j < ncol; j++) OGR_F_UnsetField(Ogr_feature, j);
    } else {
	OGR_F_UnsetField(Ogr_feature, 0);
    }
    
    /* Read & set attributes */
    if( cat >= 0 ) { /* Line with category */
	if ( doatt ) {
	    sprintf ( buf, "SELECT * FROM %s WHERE %s = %d", Fi->table,  Fi->key, cat); 
	    G_debug (2, "SQL: %s", buf);
	    db_set_string(&dbstring, buf);
	    if ( db_open_select_cursor(Driver, &dbstring, &cursor, DB_SEQUENTIAL) != DB_OK ) {
		G_fatal_error ( "Cannot select attributes for cat = %d", cat);
	    } else {
		if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK) G_fatal_error ("Cannot fetch data");
		if (!more) {
		    /* G_warning ("No database record for cat = %d", cat); */
		    /* Set at least key column to category */
		    OGR_F_SetFieldInteger ( Ogr_feature, keycol, cat );
		    noatt++;
		} else {
		    Table = db_get_cursor_table (&cursor);
		    for (j = 0; j < ncol; j++) { 
			Column = db_get_table_column (Table, j);
			Value  = db_get_column_value(Column);
			db_convert_column_value_to_string (Column, &dbstring); /* for debug only */
			G_debug (2, "col %d : val = %s", j, db_get_string (&dbstring) );
			
			colsqltype = db_get_column_sqltype(Column);
			colctype = db_sqltype_to_Ctype ( colsqltype );
			G_debug (2, "  colctype = %d", colctype );
			switch ( colctype ) {
			     case DB_C_TYPE_INT:
				OGR_F_SetFieldInteger( Ogr_feature, j, db_get_value_int(Value) );  
				break; 
			     case DB_C_TYPE_DOUBLE:
				OGR_F_SetFieldDouble( Ogr_feature, j, db_get_value_double(Value) );  
				break; 
			     case DB_C_TYPE_STRING:
				OGR_F_SetFieldString( Ogr_feature, j, db_get_value_string(Value) );  
				break; 
			     case DB_C_TYPE_DATETIME:
				db_convert_column_value_to_string (Column, &dbstring);
				OGR_F_SetFieldString( Ogr_feature, j, db_get_string (&dbstring) );  
				break; 
			}
		    }
		}
	    }

	} else { /* Use cat only */
	    OGR_F_SetFieldInteger( Ogr_feature, 0, cat );  
	}
    } else { 
	/* G_warning ( "Line without cat of field %d", field); */
	nocat++;
    }
    fout++;

    return 1;
}

/* to print available drivers in help text */
char *
OGR_list_write_drivers(void)
{
    int drn, i;
    OGRSFDriverH Ogr_driver;  
    char buf[2000];
    
    /* Open OGR DSN */
    OGRRegisterAll();
    G_debug (2, "driver count = %d", OGRGetDriverCount() ); 
    drn = -1;
    for ( i = 0; i < OGRGetDriverCount(); i++ ) {
        /* only fetch read/write drivers */
	if (OGR_Dr_TestCapability( OGRGetDriver(i),ODrCCreateDataSource) )
	{
	  Ogr_driver = OGRGetDriver( i ); 
	  G_debug (2, "driver %d/%d : %s", i, OGRGetDriverCount(), OGR_Dr_GetName ( Ogr_driver) ); 
          /* chg white space to underscore in OGR driver names */
	  sprintf (buf, "%s", OGR_Dr_GetName ( Ogr_driver) );
	  G_strchg(buf,' ','_');
	  strcat (OGRdrivers, buf);
	  if (i < OGRGetDriverCount() - 1 )
	     strcat (OGRdrivers, ",");
	}
    }
    G_debug (2, "all drivers: %s",OGRdrivers);
    return OGRdrivers;
}
