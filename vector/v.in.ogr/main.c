/****************************************************************
 *
 * MODULE:       v.in.ogr
 * 
 * AUTHOR(S):    Radim Blazek
 *               Markus Neteler (spatial parm)
 *               
 * PURPOSE:      Import OGR vectors
 *               
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 * TODO: make fixed field length of OFTIntegerList dynamic
 **************************************************************/
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
    int    i, layer, arg_s_num, nogeom;
    float  xmin=0., ymin=0., xmax=0., ymax=0.;
    int    ncols;
    struct GModule *module;
    struct Option *dsn_opt, *out_opt, *layer_opt, *spat_opt;
    char   buf[2000], namebuf[2000];
    char   *namebuf2, *namebuf3;

    /* Vector */
    struct Map_info Map;
    int    cat;

    /* Attributes */
    struct field_info *Fi;
    dbDriver *driver;
    dbHandle handle;
    dbString sql, strval;
    int dim, with_z;
    
    /* OGR */
    OGRDataSourceH Ogr_ds;
    OGRSFDriverH Ogr_driver;  
    OGRLayerH Ogr_layer;	
    OGRFieldDefnH Ogr_field; 
    char *Ogr_fieldname;
    OGRFieldType Ogr_ftype;
    OGRFeatureH Ogr_feature;  
    OGRFeatureDefnH Ogr_featuredefn;
    OGRGeometryH Ogr_geometry;
    OGRGeometryH Ogr_oRing=NULL, poSpatialFilter=NULL;

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
    layer_opt->description = "OGR layer name. If not given, available layers are printed + exit.\n"
			   "\t\tESRI Shapefile: shapefile name\n"
			   "\t\tMapInfo File: mapinfo file name";

    spat_opt = G_define_option();
    spat_opt->key = "spatial";
    spat_opt->type = TYPE_DOUBLE;
    spat_opt->multiple = YES;
    spat_opt->required = NO;
    spat_opt->description = "Import subregion only (xmin,ymin,xmax,ymax)";
    
    if (G_parser (argc, argv)) exit(-1); 

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

    if ( spat_opt->answer && layer_opt->answer ) {
        /* cut out a piece of the map */
        /* order: xmin,ymin,xmax,ymax */
        arg_s_num = 0; i = 0;
        while ( spat_opt->answers[i] ) {
	   if ( i == 0 ) xmin=atof(spat_opt->answers[i]);
	   if ( i == 1 ) ymin=atof(spat_opt->answers[i]);
	   if ( i == 2 ) xmax=atof(spat_opt->answers[i]);
	   if ( i == 3 ) ymax=atof(spat_opt->answers[i]);
           arg_s_num++; i++;
        }
        if ( arg_s_num != 4 ) G_fatal_error (" 4 parameters required for 'spatial' parameter.");
	G_debug( 2, "cut out with boundaries: %f %f %f %f",xmin,ymin,xmax,ymax);

	/* in theory this could be a irregular polygon */
	poSpatialFilter = OGR_G_CreateGeometry( wkbPolygon );
	Ogr_oRing = OGR_G_CreateGeometry( wkbLineString );
        OGR_G_AddPoint(Ogr_oRing, xmin, ymin, 0);
        OGR_G_AddPoint(Ogr_oRing, xmin, ymax, 0);
        OGR_G_AddPoint(Ogr_oRing, xmax, ymax, 0);
        OGR_G_AddPoint(Ogr_oRing, xmax, ymin, 0);
        OGR_G_AddPoint(Ogr_oRing, xmin, ymin, 0);
        OGR_G_AddGeometryDirectly(poSpatialFilter, Ogr_oRing);
	
        OGR_L_SetSpatialFilter(Ogr_layer, poSpatialFilter );
     }

    db_init_string (&sql);
    db_init_string (&strval);

    Ogr_layer = OGR_DS_GetLayer( Ogr_ds, layer );
    Ogr_featuredefn = OGR_L_GetLayerDefn( Ogr_layer );

    /* Get dimension */
    with_z = 0;
    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
        /* Geometry */
        Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
	if ( Ogr_geometry == NULL ) continue;
	dim = OGR_G_GetCoordinateDimension ( Ogr_geometry );
        OGR_F_Destroy( Ogr_feature );
	if ( dim > 2 ) {
	    fprintf (stderr, "The layer contains 3D features, 3D vector will be created.\n");
	    with_z = 1;
	    break;
	}
    }
    
    /* open output vector */
    Vect_open_new (&Map, out_opt->answer, with_z ); 
    Vect_hist_command ( &Map );

    /* Add DB link */
    Fi = Vect_default_field_info ( &Map, 1, NULL, GV_1TABLE );
    Vect_map_add_dblink ( &Map, 1, NULL, Fi->table, "cat", Fi->database, Fi->driver);

    ncols = OGR_FD_GetFieldCount( Ogr_featuredefn );
    G_debug ( 2, "%d columns\n", ncols );
    
    /* Create table */
    sprintf ( buf, "create table %s (cat integer", Fi->table );
    db_set_string ( &sql, buf);
    for ( i = 0; i < ncols; i++ ) {
	Ogr_field = OGR_FD_GetFieldDefn( Ogr_featuredefn, i );
	Ogr_ftype = OGR_Fld_GetType( Ogr_field );
	
	G_debug(3, "Ogr_ftype: %i", Ogr_ftype); /* look up below */
	
	/* auto-replace '#', '-' and '.' characters in columns with underscore for DBMI
	 * allowed are: [A-Za-z][A-Za-z0-9_]*
	 */
	sprintf(namebuf, "%s", OGR_Fld_GetNameRef( Ogr_field ));
	namebuf2      = G_strchg(namebuf , '#', '_');
	namebuf3      = G_strchg(namebuf2, '-', '_');
	Ogr_fieldname = G_strchg(namebuf3, '.', '_');

  /** Simple 32bit integer                     OFTInteger = 0        **/
  /** List of 32bit integers                   OFTIntegerList = 1    **/
  /** Double Precision floating point          OFTReal = 2           **/
  /** List of doubles                          OFTRealList = 3       **/
  /** String of ASCII chars                    OFTString = 4         **/
  /** Array of strings                         OFTStringList = 5     **/
  /** Double byte string (unsupported)         OFTWideString = 6     **/
  /** List of wide strings (unsupported)       OFTWideStringList = 7 **/
  /** Raw Binary data (unsupported)            OFTBinary = 8         **/

	if( Ogr_ftype == OFTInteger ) { 
	    sprintf (buf, ", %s integer", Ogr_fieldname );
	} else if( Ogr_ftype == OFTIntegerList ) {
	    /* hack: treat as string */
	    sprintf (buf, ", %s varchar ( %d )", Ogr_fieldname, 40 );
	    G_warning ( "Writing column <%s> with fixed length 40 chars (may be truncated)", Ogr_fieldname);
	} else if( Ogr_ftype == OFTReal ) { 
	    sprintf (buf, ", %s double precision", Ogr_fieldname );
	} else if( Ogr_ftype == OFTString ) { 
	    sprintf (buf, ", %s varchar ( %d )", Ogr_fieldname, OGR_Fld_GetWidth(Ogr_field) );
	} else if( Ogr_ftype == OFTStringList ) {
	    /* hack: treat as string */
	    sprintf (buf, ", %s varchar ( %d )", Ogr_fieldname, 40 );
	    G_warning ( "Writing column <%s> with fixed length 40 chars (may be truncated)", Ogr_fieldname);
	} else {
	    G_warning ( "Column type not supported (%s)", Ogr_fieldname );
	    buf[0] = 0;
	}
	db_append_string ( &sql, buf);
    }
    db_append_string ( &sql, ")" );
    G_debug ( 3, db_get_string ( &sql ) );

    driver = db_start_driver( Fi->driver );
    if (driver == NULL) G_fatal_error ( "Cannot open driver %s", Fi->driver );
    db_init_handle (&handle);
    db_set_handle (&handle, Vect_subst_var(Fi->database,&Map), NULL);
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
    nogeom = 0;
    OGR_L_ResetReading ( Ogr_layer ); 
    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
        /* Geometry */
        Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
	if ( Ogr_geometry == NULL ) {
	    nogeom++;
	} else {
	    geom ( Ogr_geometry, &Map, cat );
	}

	/* Attributes */
	sprintf ( buf, "insert into %s values ( %d", Fi->table, cat );
	db_set_string ( &sql, buf);
	for ( i = 0; i < ncols; i++ ) { 
	    Ogr_field = OGR_FD_GetFieldDefn( Ogr_featuredefn, i );
	    Ogr_ftype = OGR_Fld_GetType( Ogr_field );
	    if( OGR_F_IsFieldSet( Ogr_feature, i ) ) {
		if( Ogr_ftype == OFTInteger || Ogr_ftype == OFTReal ) { 
		    sprintf (buf, ", %s", OGR_F_GetFieldAsString( Ogr_feature, i) );
		} else if( Ogr_ftype == OFTString || Ogr_ftype == OFTIntegerList ) { 
                    db_set_string ( &strval,  (char *) OGR_F_GetFieldAsString( Ogr_feature, i) );
		    db_double_quote_string (&strval);
		    sprintf (buf, ", '%s'", db_get_string(&strval) );
		}
	 
	    } else {
		/* G_warning ( "Column value not set" ); */

		/* TODO: change to 'NULL' once supported by dbf driver */
		if( Ogr_ftype == OFTInteger || Ogr_ftype == OFTReal ) { 
		    sprintf (buf, ", 0" );
		} else if( Ogr_ftype == OFTString || Ogr_ftype == OFTIntegerList ) { 
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

    if ( nogeom > 0 )
	G_warning ("%d feature without geometry.", nogeom);
    
    OGR_DS_Destroy( Ogr_ds );
    
    db_close_database(driver);
    db_shutdown_driver(driver);
    
    Vect_build ( &Map, stdout );
    if (Vect_get_num_areas(&Map) > 0)
       G_warning ("Boundaries in <%s> are not cleaned. Run (suggestion):\n v.clean in=%s out=%s_clean tool=bpol,rmdupl", out_opt->answer, out_opt->answer, out_opt->answer);

    Vect_close ( &Map );

    exit(0) ;
}
