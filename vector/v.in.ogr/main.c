/* ***************************************************************
 * *
 * * MODULE:       v.in.ogr
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Import OGR vectors
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

int geom(OGRGeometryH hGeom, struct Map_info *Map, int cat );

int 
main (int argc, char *argv[])
{
    int    i, layer;
    int    ncols;
    struct GModule *module;
    struct Option *dsn_opt, *out_opt, *layer_opt;
    char   buf[2000];

    /* Vector */
    struct Map_info Map;
    int    cat;

    /* Attributes */
    struct field_info *Fi;
    dbDriver *driver;
    dbHandle handle;
    dbString sql, strval;
    
    /* OGR */
    OGRDataSourceH Ogr_ds;
    OGRSFDriverH Ogr_driver;  
    OGRLayerH Ogr_layer;	
    OGRFieldDefnH Ogr_field; 
    OGRFieldType Ogr_ftype;
    OGRFeatureH Ogr_feature;  
    OGRFeatureDefnH Ogr_featuredefn;
    OGRGeometryH Ogr_geometry;

    G_gisinit(argv[0]);

    OGRRegisterAll();
    /* Module options */
    sprintf ( buf, "Convert OGR vectors to GRASS. Available drivers:\n" );
    for ( i = 0; i < OGRGetDriverCount(); i++ ) {
	Ogr_driver = OGRGetDriver( i );  
	sprintf (buf, "%s      %s\n", buf, OGR_Dr_GetName ( Ogr_driver) ); 
    }
    module = G_define_module();
    module->description = G_store(buf);


    dsn_opt = G_define_option();
    dsn_opt->key = "dsn";
    dsn_opt->type =  TYPE_STRING;
    dsn_opt->required = YES;
    dsn_opt->description = "OGR datasource name.\n"
			   "\t\tESRI Shapefile: directory containing shapefiles\n"
			   "\t\tMapInfo File: directory containing mapinfo files";

    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    layer_opt = G_define_option();
    layer_opt->key = "layer";
    layer_opt->type = TYPE_STRING;
    layer_opt->required = NO;
    layer_opt->description = "OGR layer name. In not given, print available layers.\n"
			   "\t\tESRI Shapefile: shapefile name\n"
			   "\t\tMapInfo File: mapinfo file name";
    
    if (G_parser (argc, argv)) exit(-1); 

    G_warning ("Area boundaries are not cleaned by this module. Run v.clean on imported vector"
	       " if it contains areas.");
    
    db_init_string (&sql);
    db_init_string (&strval);
    
    /* open output vector */
    Vect_open_new (&Map, out_opt->answer, 0 ); 
    Vect_hist_command ( &Map );

    /* Add DB link */
    Fi = Vect_default_field_info ( Map.name, 1, NULL, GV_1TABLE );
    Vect_map_add_dblink ( &Map, 1, NULL, Fi->table, "cat", Fi->database, Fi->driver);

    /* Open OGR DSN */
    Ogr_ds = OGROpen( dsn_opt->answer, FALSE, NULL );
    if( Ogr_ds == NULL ) G_fatal_error ("Cannot open data source");
    if ( !layer_opt->answer )
        fprintf ( stdout, "Data source contains %d layers:\n", OGR_DS_GetLayerCount(Ogr_ds) );
    
    layer = -1;
    for ( i = 0; i < OGR_DS_GetLayerCount(Ogr_ds); i++ ) {
	Ogr_layer =  OGR_DS_GetLayer( Ogr_ds, i );
	Ogr_featuredefn = OGR_L_GetLayerDefn( Ogr_layer );
        if ( !layer_opt->answer ) {
	    if ( i > 0 ) fprintf ( stdout, ", " );
	    fprintf ( stdout, "%s", OGR_FD_GetName( Ogr_featuredefn ) );
	} else { 
	    if ( strcmp(OGR_FD_GetName(Ogr_featuredefn), layer_opt->answer) == 0 ) {
		layer = i;
	    }
	}
    }
    if ( !layer_opt->answer ) {
        fprintf ( stdout, "\nPlease specify a layer to be imported.\n" );
	exit (0);
    }
    if ( layer == -1 ) G_fatal_error ("Layer not found");

    Ogr_layer = OGR_DS_GetLayer( Ogr_ds, layer );
    Ogr_featuredefn = OGR_L_GetLayerDefn( Ogr_layer );

    ncols = OGR_FD_GetFieldCount( Ogr_featuredefn );
    G_debug ( 2, "%d columns\n", ncols );
    
    /* Create table */
    sprintf ( buf, "create table %s (cat integer", Fi->table );
    db_set_string ( &sql, buf);
    for ( i = 0; i < ncols; i++ ) {
	Ogr_field = OGR_FD_GetFieldDefn( Ogr_featuredefn, i );
	Ogr_ftype = OGR_Fld_GetType( Ogr_field );

	if( Ogr_ftype == OFTInteger ) { 
	    sprintf (buf, ", %s integer", OGR_Fld_GetNameRef( Ogr_field ) );
	} else if( Ogr_ftype == OFTReal ) { 
	    sprintf (buf, ", %s double precision", OGR_Fld_GetNameRef( Ogr_field ) );
	} else if( Ogr_ftype == OFTString ) { 
	    sprintf (buf, ", %s varchar ( %d )", OGR_Fld_GetNameRef(Ogr_field), OGR_Fld_GetWidth(Ogr_field) );
	} else {
	    G_warning ( "Column type not supported (%s)", OGR_Fld_GetNameRef( Ogr_field ) );
	    buf[0] = 0;
	}
	db_append_string ( &sql, buf);
    }
    db_append_string ( &sql, ")" );
    G_debug ( 3, db_get_string ( &sql ) );

    driver = db_start_driver( Fi->driver );
    if (driver == NULL) G_fatal_error ( "Cannot open driver %s", Fi->driver );
    db_init_handle (&handle);
    db_set_handle (&handle, Vect_subst_var(Fi->database,Map.name,G_mapset()), NULL);
    if (db_open_database(driver, &handle) != DB_OK) {
	db_shutdown_driver(driver);
	G_fatal_error ( "Cannot open database %s", Fi->database );
    }
    if (db_execute_immediate (driver, &sql) != DB_OK ) {
	db_close_database(driver);
	db_shutdown_driver(driver);
	G_fatal_error ( "Cannot create table: %s", db_get_string ( &sql )  );
    }

    /* Import feature */
    cat = 1;
    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
        /* Geometry */
        Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
	if ( Ogr_geometry == NULL ) {
	    G_warning ("Feature without geometry");
	    continue;
	}
        geom ( Ogr_geometry, &Map, cat );

	/* Attributes */
	sprintf ( buf, "insert into %s values ( %d", Fi->table, cat );
	db_set_string ( &sql, buf);
	for ( i = 0; i < ncols; i++ ) { 
	    Ogr_field = OGR_FD_GetFieldDefn( Ogr_featuredefn, i );
	    Ogr_ftype = OGR_Fld_GetType( Ogr_field );
	    if( OGR_F_IsFieldSet( Ogr_feature, i ) ) {
		if( Ogr_ftype == OFTInteger || Ogr_ftype == OFTReal ) { 
		    sprintf (buf, ", %s", OGR_F_GetFieldAsString( Ogr_feature, i) );
		} else if( Ogr_ftype == OFTString ) { 
                    db_set_string ( &strval,  (char *) OGR_F_GetFieldAsString( Ogr_feature, i) );
		    db_double_quote_string (&strval);
		    sprintf (buf, ", '%s'", db_get_string(&strval) );
		}
	 
	    } else {
		/* G_warning ( "Column value not set" ); */

		/* TODO: change to 'NULL' once supported by dbf driver */
		if( Ogr_ftype == OFTInteger || Ogr_ftype == OFTReal ) { 
		    sprintf (buf, ", 0" );
		} else if( Ogr_ftype == OFTString ) { 
		    sprintf (buf, ", ''" );
		}
		/* sprintf (buf, ", NULL" ); */
	    }
	    db_append_string ( &sql, buf);
	}
	db_append_string ( &sql, " )" );
	G_debug ( 3, db_get_string ( &sql ) );

	if (db_execute_immediate (driver, &sql) != DB_OK ) {
	    db_close_database(driver);
	    db_shutdown_driver(driver);
	    G_fatal_error ( "Cannot insert new row: %s", db_get_string ( &sql )  );
	}
	 
        OGR_F_Destroy( Ogr_feature );
	cat++;
    }
    
    OGR_DS_Destroy( Ogr_ds );
    
    db_close_database(driver);
    db_shutdown_driver(driver);
    
    Vect_build ( &Map, stdout );
    Vect_close ( &Map );

    exit(0) ;
}

