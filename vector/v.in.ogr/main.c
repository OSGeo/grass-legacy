/****************************************************************
 *
 * MODULE:       v.in.ogr
 * 
 * AUTHOR(S):    Radim Blazek
 *               Markus Neteler (spatial parm, projection support)
 *               Paul Kelly (projection support)
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
#define MAIN
#include "config.h"
#include <stdlib.h> 
#include <string.h> 
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "ogr_api.h"
#include "global.h"
#include "gprojects.h"

int geom(OGRGeometryH hGeom, struct Map_info *Map, int field, int cat, double min_area, int type, int mk_centr );
int centroid(OGRGeometryH hGeom, CENTR *Centr, SPATIAL_INDEX *Sindex, int field, int cat, double min_area, int type);

int 
main (int argc, char *argv[])
{
    int    i, j, layer, arg_s_num, nogeom, ncnames;
    float  xmin=0., ymin=0., xmax=0., ymax=0.;
    int    ncols, type;
    struct GModule *module;
    double min_area, snap;
    struct Option *dsn_opt, *out_opt, *layer_opt, *spat_opt, *min_area_opt;
    struct Option *snap_opt, *type_opt, *outloc_opt, *cnames_opt;
    struct Flag *list_flag, *no_clean_flag, *z_flag, *notab_flag;
    struct Flag *over_flag, *extend_flag, *formats_flag;
    char   buf[2000], namebuf[2000];
    char   *separator;
    struct Key_Value *loc_proj_info = NULL, *loc_proj_units = NULL;
    struct Key_Value *proj_info, *proj_units;
    struct Cell_head cellhd, loc_wind;
    char   error_msg[8192];

    /* Vector */
    struct Map_info Map;
    int    cat;

    /* Attributes */
    struct field_info *Fi;
    dbDriver *driver;
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
    OGRGeometryH Ogr_geometry, Ogr_oRing=NULL, poSpatialFilter=NULL;
    OGRSpatialReferenceH Ogr_projection;
    OGREnvelope oExt;
    char **layer_names; /* names of layers to be imported */
    int *layers;  /* layer indexes */
    int nlayers; /* number of layers to import */
    char **available_layer_names; /* names of layers to be imported */
    int navailable_layers; 
    int layer_id;
    int overwrite;
    char *overstr;

    G_gisinit(argv[0]);

    G_begin_polygon_area_calculations(); /* Used in geom() */

    OGRRegisterAll();
    /* Module options */
    sprintf ( buf, "Convert OGR vectors to GRASS. Available drivers:\n" );
    for ( i = 0; i < OGRGetDriverCount(); i++ ) {
	Ogr_driver = OGRGetDriver( i );
	if ( i== 0) 
	  sprintf (buf, "%s%s", buf, OGR_Dr_GetName ( Ogr_driver) ); 
	else
	  sprintf (buf, "%s,%s", buf, OGR_Dr_GetName ( Ogr_driver) ); 
    }
    module = G_define_module();
    module->description = G_store(buf);

    dsn_opt = G_define_option();
    dsn_opt->key = "dsn";
    dsn_opt->type =  TYPE_STRING;
    dsn_opt->required = YES;
    dsn_opt->gisprompt = "file,file,file";
    dsn_opt->description = "OGR datasource name.\n"
			   "\t\tESRI Shapefile: directory containing shapefiles\n"
			   "\t\tMapInfo File: directory containing mapinfo files";

    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    layer_opt = G_define_option();
    layer_opt->key = "layer";
    layer_opt->type = TYPE_STRING;
    layer_opt->required = NO;
    layer_opt->multiple = YES;
    layer_opt->description = "OGR layer name. If not given, available layers are printed + exit.\n"
			   "\t\tESRI Shapefile: shapefile name\n"
			   "\t\tMapInfo File: mapinfo file name";

    spat_opt = G_define_option();
    spat_opt->key = "spatial";
    spat_opt->type = TYPE_DOUBLE;
    spat_opt->multiple = YES;
    spat_opt->required = NO;
    spat_opt->description = "Import subregion only (xmin,ymin,xmax,ymax  - usually W,S,E,N)";

    min_area_opt = G_define_option();
    min_area_opt->key = "min_area";
    min_area_opt->type = TYPE_DOUBLE;
    min_area_opt->required = NO;
    min_area_opt->answer = "0.0001";
    min_area_opt->description = "Minimum size of area to be imported (square units). Smaller areas and "
	                        "islands are ignored. Should be greater than snap^2.";

    type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
    type_opt->options = "point,line,boundary,centroid";
    type_opt->answer = "";
    type_opt->description = "Optionaly change default input type:\n"
	      "\t point -> import area centroids as points\n"
	      "\t line -> import area boundaries as centroids\n"
	      "\t boundary -> import lines as area boundaries\n"
	      "\t centroid -> import points as centroids";
    
    snap_opt = G_define_option();
    snap_opt->key = "snap";
    snap_opt->type = TYPE_DOUBLE;
    snap_opt->required = NO;
    snap_opt->answer = "-1";
    snap_opt->description = "Snapping threshold for boundaries. -1 for no snap.";

    outloc_opt = G_define_option();
    outloc_opt->key = "location";
    outloc_opt->type = TYPE_STRING;
    outloc_opt->required = NO;
    outloc_opt->description = "Name for new location to create";

    cnames_opt = G_define_option();
    cnames_opt->key = "cnames";
    cnames_opt->type = TYPE_STRING;
    cnames_opt->required = NO;
    cnames_opt->multiple = YES;
    cnames_opt->description = "List of column names to be used instead of original names, "
	                      "first is used for category column.";

    list_flag = G_define_flag ();
    list_flag->key             = 'l';
    list_flag->description     = "List available layers in data source and exit.";

    formats_flag = G_define_flag ();
    formats_flag->key  	      = 'f';
    formats_flag->description = "List available formats and exit.";

    no_clean_flag = G_define_flag ();
    no_clean_flag->key             = 'c';
    no_clean_flag->description     = "Do not clean polygons.";
    
    z_flag = G_define_flag ();
    z_flag->key             = 'z';
    z_flag->description     = "Create 3D output.";
    
    notab_flag = G_define_flag ();
    notab_flag->key             = 't';
    notab_flag->description     = "Do not create attribute table.";

    over_flag = G_define_flag();
    over_flag->key = 'o';
    over_flag->description = "Override projection (use location's projection).";

    extend_flag = G_define_flag();
    extend_flag->key = 'e';
    extend_flag->description = "Extend location extents based on new dataset.";

    /* The parser checks if the map already exists in current mapset, this is wrong 
     * if location options is used, so we switch out the check and do it in the module after the parser */
    overwrite = 0;
    if ( (overstr = G__getenv ( "OVERWRITE" )) ) {
	overwrite = atoi ( overstr );
    }
    G__setenv ( "OVERWRITE", "1" ); 

    if (G_parser (argc, argv)) exit(-1); 
        
    if ( !outloc_opt->answer && !overwrite ) { /* Check if the map exists */
	if ( G_find_vector2 (out_opt->answer, G_mapset()) ) {
	    G_fatal_error ( "The vector '%s' already exists.", out_opt->answer );
	}
    }

    min_area = atof (min_area_opt->answer);
    snap = atof (snap_opt->answer);
    type = Vect_option_to_types ( type_opt );

    ncnames = 0;
    if ( cnames_opt->answers ) {
	i = 0;
	while ( cnames_opt->answers[i++] ) {
	    ncnames++;
	}
    }

    if(formats_flag->answer) {
	int iDriver;
	fprintf(stdout, "Available OGR Drivers:\n" );

	for(iDriver = 0; iDriver < OGRGetDriverCount(); iDriver++) {
	    OGRSFDriverH *poDriver = OGRGetDriver(iDriver);
	    fprintf(stdout, "  %s\n", OGR_Dr_GetName(poDriver));

/* TODO: read/write check:
	    if( OGR_Dr_TestCapability(poDriver, "??") )
		fprintf(stdout, " %s (read/write)\n",
		    OGR_Dr_GetName(poDriver) );
	    else
		fprintf(stdout, "  %s (read only)\n",
		    OGR_Dr_GetName(poDriver) );
*/
	}
	exit(0);
    }

    /* Open OGR DSN */
    Ogr_ds = OGROpen( dsn_opt->answer, FALSE, NULL );
    if( Ogr_ds == NULL ) G_fatal_error ("Cannot open data source: %s", dsn_opt->answer);

    /* Make a list of available layers */
    navailable_layers = OGR_DS_GetLayerCount(Ogr_ds);
    available_layer_names = (char **) G_malloc ( navailable_layers * sizeof (char *) );
    
    if ( list_flag->answer )
        fprintf ( stdout, "Data source contains %d layers:\n", navailable_layers );
    
    for ( i = 0; i < navailable_layers; i++ ) {
	Ogr_layer =  OGR_DS_GetLayer( Ogr_ds, i );
	Ogr_featuredefn = OGR_L_GetLayerDefn( Ogr_layer );
	available_layer_names[i] = G_store ( (char *)OGR_FD_GetName( Ogr_featuredefn ) );
	
	if ( list_flag->answer ) {
	    if ( i > 0 ) fprintf ( stdout, ", " );
	    fprintf ( stdout, "%s", available_layer_names[i] );
	}
    }
    
    if ( list_flag->answer ) {
	fprintf ( stdout, "\n" );
        exit(0) ;
    }

    /* Make a list of layers to be imported */
    if ( layer_opt->answer ) { /* From option */
	nlayers = 0;
	while ( layer_opt->answers[nlayers] ) 
	    nlayers++;

	layer_names = (char **) G_malloc ( nlayers * sizeof (char *) );
	layers = (int *) G_malloc ( nlayers * sizeof (int) );

	for ( i = 0; i < nlayers; i++ ) {
	    layer_names[i] = G_store ( layer_opt->answers[i] );
	    /* Find it in the source */
	    layers[i] = -1;
	    for ( j = 0; j < navailable_layers; j++ ) {
		if ( strcmp( available_layer_names[j], layer_names[i]) == 0 ) {
		    layers[i] = j; 
		    break;
		}
	    }
	    if ( layers[i] == -1 )
                G_fatal_error ("Layer '%s' not available", layer_names[i]);
	}
    } else { /* use list of all layers */
	nlayers = navailable_layers;
	layer_names = available_layer_names;
	layers = (int *) G_malloc ( nlayers * sizeof (int) );
	for ( i = 0 ; i < nlayers; i++ )
	    layers[i] = i;
    }

    if ( spat_opt->answer ) {
        /* See as reference: gdal/ogr/ogr_capi_test.c */
	
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
	G_debug( 2, "cut out with boundaries: xmin:%f ymin:%f xmax:%f ymax:%f",xmin,ymin,xmax,ymax);

	/* in theory this could be an irregular polygon */
	poSpatialFilter = OGR_G_CreateGeometry( wkbPolygon );
        Ogr_oRing = OGR_G_CreateGeometry( wkbLinearRing );
        OGR_G_AddPoint(Ogr_oRing, xmin, ymin, 0.0);
        OGR_G_AddPoint(Ogr_oRing, xmin, ymax, 0.0);
        OGR_G_AddPoint(Ogr_oRing, xmax, ymax, 0.0);
        OGR_G_AddPoint(Ogr_oRing, xmax, ymin, 0.0);
        OGR_G_AddPoint(Ogr_oRing, xmin, ymin, 0.0);
        OGR_G_AddGeometryDirectly(poSpatialFilter, Ogr_oRing);
	
        OGR_L_SetSpatialFilter(Ogr_layer, poSpatialFilter );
     }

    /* fetch boundaries */
    if ( (OGR_L_GetExtent (Ogr_layer , &oExt, 1 )) == OGRERR_NONE ) {
	G_get_window ( &cellhd );
        cellhd.north  = oExt.MaxY;
        cellhd.south  = oExt.MinY;
        cellhd.west   = oExt.MinX;
        cellhd.east   = oExt.MaxX;
        cellhd.rows   = 20 ; /* TODO - calculate useful values */
        cellhd.cols   = 20 ;
        cellhd.ns_res = (cellhd.north-cellhd.south)/cellhd.rows;
        cellhd.ew_res = (cellhd.east-cellhd.west)/cellhd.cols;
    }
    else {
        cellhd.north  = 1.;
        cellhd.south  = 0.;
        cellhd.west   = 0.;
        cellhd.east   = 1.;
        cellhd.top    = 1.;
        cellhd.bottom = 1.;
        cellhd.rows   = 1 ;
        cellhd.rows3  = 1 ;
        cellhd.cols   = 1 ;
        cellhd.cols3  = 1 ;
        cellhd.depths = 1 ;
        cellhd.ns_res = 1.;
        cellhd.ns_res3= 1.;
        cellhd.ew_res = 1.;
        cellhd.ew_res3= 1.;
        cellhd.tb_res = 1.;
    }
   
    /* Fetch input map projection in GRASS form. */
    proj_info = NULL;
    proj_units = NULL;
    Ogr_projection = OGR_L_GetSpatialRef(Ogr_layer);
 
    /* Do we need to create a new location? */
    if( outloc_opt->answer != NULL )
    {
        /* Convert projection information interactively as it is important
	 * to set up datum etc. */
        if ( GPJ_osr_to_grass( &cellhd, &proj_info, 
			       &proj_units, Ogr_projection, 1) < 0 )
	    G_fatal_error("Unable to convert input map projection to GRASS "
			  "format; cannot create new location.");
	else		  
            G_make_location( outloc_opt->answer, &cellhd,
			     proj_info, proj_units, NULL );
    }
    else
    {
        /* Projection only required for checking so convert non-interactively */
        if ( GPJ_osr_to_grass( &cellhd, &proj_info, 
			       &proj_units, Ogr_projection, 0) < 0 )
            G_warning("Unable to convert input map projection information to "
		      "GRASS format for checking");

        /* Does the projection of the current location match the dataset? */
        /* G_get_window seems to be unreliable if the location has been changed */
        G__get_window ( &loc_wind, "", "DEFAULT_WIND", "PERMANENT");
        /* fetch LOCATION PROJ info */
        if (loc_wind.proj != PROJECTION_XY) {
            loc_proj_info = G_get_projinfo();
            loc_proj_units = G_get_projunits();
	}       
    
        if( over_flag->answer )
        {
            cellhd.proj = loc_wind.proj;
            cellhd.zone = loc_wind.zone;
	    fprintf(stderr, "Over-riding projection check.\n"
		            "Proceeding with import...\n");
        } 
        else if( loc_wind.proj != cellhd.proj
                   || !G_compare_projections( loc_proj_info, loc_proj_units, 
                                              proj_info, proj_units ) )
        {
            int     i_value;
    
            strcpy( error_msg, 
                    "Projection of dataset does not"
                    " appear to match current location.\n\n");

    /* TODO: output this info sorted by key: */
            if( loc_proj_info != NULL )
            {
                strcat( error_msg, "LOCATION PROJ_INFO is:\n" );
                for( i_value = 0; 
                     loc_proj_info != NULL && i_value < loc_proj_info->nitems; 
                     i_value++ )
                    sprintf( error_msg + strlen(error_msg), "%s: %s\n", 
                             loc_proj_info->key[i_value],
                             loc_proj_info->value[i_value] );
                strcat( error_msg, "\n" );
            }
    
            if( proj_info != NULL )
            {
                strcat( error_msg, "Dataset PROJ_INFO is:\n" );
                for( i_value = 0; 
                     proj_info != NULL && i_value < proj_info->nitems; 
                     i_value++ )
                    sprintf( error_msg + strlen(error_msg), "%s: %s\n", 
                             proj_info->key[i_value],
                             proj_info->value[i_value] );
            }
            else
            {
                if( cellhd.proj == PROJECTION_XY )
                    sprintf( error_msg + strlen(error_msg), 
                             "cellhd.proj = %d (unreferenced)\n", 
                             cellhd.proj );
                else if( cellhd.proj == PROJECTION_LL )
                    sprintf( error_msg + strlen(error_msg), 
                             "cellhd.proj = %d (lat/long)\n", 
                             cellhd.proj );
                else if( cellhd.proj == PROJECTION_UTM )
                    sprintf( error_msg + strlen(error_msg), 
                             "cellhd.proj = %d (UTM), zone = %d\n", 
                             cellhd.proj, cellhd.zone );
                else if( cellhd.proj == PROJECTION_SP )
                    sprintf( error_msg + strlen(error_msg), 
                             "cellhd.proj = %d (State Plane), zone = %d\n", 
                             cellhd.proj, cellhd.zone );
                else 
                    sprintf( error_msg + strlen(error_msg), 
                             "cellhd.proj = %d (unknown), zone = %d\n", 
                             cellhd.proj, cellhd.zone );
            }
            sprintf( error_msg + strlen(error_msg), 
    	             "\nYou can use the -o flag to %s to override this check.\n",
    	    	     G_program_name() );
            strcat( error_msg, 
             "Consider to generate a new location with 'location' parameter"
             " from input data set.\n" );
            G_fatal_error( error_msg );
        }
        else
	    fprintf(stderr, "Projection of input dataset and current location "
		            "appear to match.\nProceeding with import...\n");
   
    }
    OSRDestroySpatialReference(Ogr_projection);
   
    db_init_string (&sql);
    db_init_string (&strval);
	
    /* open output vector */
    if ( z_flag->answer )
        Vect_open_new (&Map, out_opt->answer, 1 ); 
    else 
        Vect_open_new (&Map, out_opt->answer, 0 ); 

    Vect_hist_command ( &Map );

    /* Points and lines are written immediately with categories. Boundaries of polygons are
     * written to the vector then cleaned and centroids are calculated for all areas in cleaan vector.
     * Then second pass through finds all centroids in each polygon feature and adds its category
     * to the centroid. The result is that one centroids may have 0, 1 ore more categories
     * of one ore more (more input layers) fields. */
    with_z = 0;
    for ( layer = 0; layer < nlayers; layer++ ) {
	fprintf (stderr, "Layer: %s\n", layer_names[layer]);
	layer_id = layers[layer];

	Ogr_layer = OGR_DS_GetLayer( Ogr_ds, layer_id );
	Ogr_featuredefn = OGR_L_GetLayerDefn( Ogr_layer );
	
	/* Add DB link */
	if ( !notab_flag->answer ) {
	    char *cat_col_name = "cat";
	    
	    if ( nlayers == 1 ) { /* one layer only */
		Fi = Vect_default_field_info ( &Map, layer+1, NULL, GV_1TABLE );
	    } else {
		Fi = Vect_default_field_info ( &Map, layer+1, NULL, GV_MTABLE );
	    }
	
     	    if ( ncnames > 0 ) {
    		cat_col_name = cnames_opt->answers[0];
	    }
	    Vect_map_add_dblink ( &Map, layer+1, NULL, Fi->table, cat_col_name, Fi->database, Fi->driver);

	    ncols = OGR_FD_GetFieldCount( Ogr_featuredefn );
	    G_debug ( 2, "%d columns", ncols );
	    
	    /* Create table */
	    sprintf ( buf, "create table %s (%s integer", Fi->table, cat_col_name );
	    db_set_string ( &sql, buf);
	    for ( i = 0; i < ncols; i++ ) {
		char *c;
		    
		Ogr_field = OGR_FD_GetFieldDefn( Ogr_featuredefn, i );
		Ogr_ftype = OGR_Fld_GetType( Ogr_field );
		
		G_debug(3, "Ogr_ftype: %i", Ogr_ftype); /* look up below */

		if ( i < ncnames-1 ) {
		    Ogr_fieldname = cnames_opt->answers[i+1];
		} else {
		    /* Change column names to [A-Za-z][A-Za-z0-9_]* */
		    Ogr_fieldname = strdup ( OGR_Fld_GetNameRef( Ogr_field ) );
		    G_debug(3, "Ogr_fieldname: '%s'", Ogr_fieldname);

		    c = Ogr_fieldname;
		    while ( *c ) {
			*c = toascii(*c);

			if ( !( *c>='A' && *c<='Z' ) && !( *c>='a' && *c<='z' ) && !( *c>='0' && *c<='9' ) ) {
			    *c = '_';
			}
			c++;
		    }

		    c = Ogr_fieldname;
		    if ( !( *c>='A' && *c<='Z' ) && !( *c>='a' && *c<='z' ) ) { 
			*c = 'x';
		    }
		    G_debug(3, "Ogr_fieldname: '%s'", Ogr_fieldname);

		}
		
		/* avoid that we get the 'cat' column twice */
		if ( strcmp(Ogr_fieldname, "cat") == 0 ) {
		    sprintf(namebuf, "%s_", Ogr_fieldname);
		    Ogr_fieldname = strdup ( namebuf );
		}
		    
		if ( strcmp ( OGR_Fld_GetNameRef( Ogr_field ), Ogr_fieldname) != 0 ) {
		    G_warning ("Column name changed: '%s' -> '%s'",  
				  OGR_Fld_GetNameRef( Ogr_field ), Ogr_fieldname );
		}
		
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
		    int fwidth;
		    fwidth = OGR_Fld_GetWidth(Ogr_field); 
		    /* TODO: read all records first and find the longest string length */
		    if ( fwidth == 0) {
			G_warning ("Width for column '%s' set to 255 (was not specified by OGR), "
				   "some strings may be truncated!", Ogr_fieldname );
			fwidth = 255;
		    }
		    sprintf (buf, ", %s varchar ( %d )", Ogr_fieldname, fwidth );
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

	    driver = db_start_driver_open_database ( Fi->driver, Vect_subst_var(Fi->database,&Map) );
	    if ( driver == NULL ) {
	        G_fatal_error ( "Cannot open database %s by driver %s", 
			             Vect_subst_var(Fi->database,&Map), Fi->driver );
	    }
	    
	    if (db_execute_immediate (driver, &sql) != DB_OK ) {
		db_close_database(driver);
		db_shutdown_driver(driver);
		G_fatal_error ( "Cannot create table: %s", db_get_string ( &sql )  );
	    }

	    if ( db_create_index2(driver, Fi->table, cat_col_name ) != DB_OK )
		G_warning ( "Cannot create index" );

	    if (db_grant_on_table (driver, Fi->table, DB_PRIV_SELECT, DB_GROUP|DB_PUBLIC ) != DB_OK )
		G_fatal_error ( "Cannot grant privileges on table %s", Fi->table );

	    db_begin_transaction ( driver );
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
	        dim = OGR_G_GetCoordinateDimension ( Ogr_geometry );
		if ( dim > 2 ) 
		    with_z = 1;

		geom ( Ogr_geometry, &Map, layer+1, cat, min_area, type, no_clean_flag->answer );
	    }

	    /* Attributes */
	    if ( !notab_flag->answer ) {
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
	    }
	     
	    OGR_F_Destroy( Ogr_feature );
	    cat++;
	}
	
	if ( !notab_flag->answer ) {
	    db_commit_transaction ( driver );
	    db_close_database_shutdown_driver ( driver );
	}

	if ( nogeom > 0 )
	    G_warning ("%d %s without geometry.", nogeom, nogeom == 1 ? "feature" : "features" );
    }
    
    
    separator = "-----------------------------------------------------\n";
    fprintf ( stderr, separator );
    Vect_build ( &Map, stderr );
    
    if ( !no_clean_flag->answer && Vect_get_num_primitives(&Map, GV_BOUNDARY) > 0) {
	int ret, centr, ncentr, otype, n_overlaps, n_nocat;
        CENTR  *Centr;
	SPATIAL_INDEX si;
	double x, y, total_area, overlap_area, nocat_area;
	BOUND_BOX box;
	struct line_pnts *Points;
	int nmodif;

	Points = Vect_new_line_struct ();

        fprintf ( stderr, separator );
	G_warning ( "Cleaning polygons, result is not guaranteed!\n");

        Vect_close ( &Map );
	Vect_open_update (&Map, out_opt->answer, G_mapset());
        Vect_build_partial ( &Map, GV_BUILD_BASE, stderr ); /* Downgrade topo */
	
	if ( snap >= 0 ) {
	    fprintf ( stderr, separator );
	    fprintf ( stderr, "Snap boundaries (threshold = %.3e):\n", snap );
	    Vect_snap_lines ( &Map, GV_BOUNDARY, snap, NULL, stderr ); 
	}

	/* It is not to clean to snap centroids, but I have seen data with 2 duplicate polygons
	 * (as far as decimal places were printed) and centroids were not identical */
	/* Disabled, because overlapping polygons result in many duplicate centroids anyway */
	/*
        fprintf ( stderr, separator );
	fprintf ( stderr, "Snap centroids (threshold 0.000001):\n" );
	Vect_snap_lines ( &Map, GV_CENTROID, 0.000001, NULL, stderr ); 
	*/
	
        fprintf ( stderr, separator );
	fprintf ( stderr, "Break polygons:\n" );
	Vect_break_polygons ( &Map, GV_BOUNDARY, NULL, stderr ); 
	
	/* It is important to remove also duplicate centroids in case of duplicate imput polygons */
        fprintf ( stderr, separator );
	fprintf ( stderr, "Remove duplicates:\n" );
	Vect_remove_duplicates ( &Map, GV_BOUNDARY | GV_CENTROID, NULL, stderr ); 
	
	/* Vect_clean_small_angles_at_nodes() can change the geometry so that new intersections
	 * are created. We must call Vect_break_lines(), Vect_remove_duplicates()
	 * and Vect_clean_small_angles_at_nodes() until no more small dangles are found */
	do {
	    fprintf ( stderr, separator );
	    fprintf ( stderr, "Break boundaries:\n" );
	    Vect_break_lines ( &Map, GV_BOUNDARY, NULL, stderr ); 

	    fprintf ( stderr, separator );
	    fprintf ( stderr, "Remove duplicates:\n" );
	    Vect_remove_duplicates ( &Map, GV_BOUNDARY, NULL, stderr ); 

	    fprintf ( stderr, separator );
	    fprintf ( stderr, "Clean boundaries at nodes:\n" );
	    nmodif = Vect_clean_small_angles_at_nodes ( &Map, GV_BOUNDARY, NULL, stderr );
	} while ( nmodif > 0 );

        fprintf ( stderr, separator );
	if ( type & GV_BOUNDARY ) { /* that means lines were converted boundaries */
	    fprintf ( stderr, "Change boundary dangles to lines:\n" );
	    Vect_chtype_dangles ( &Map, -1.0, NULL, stderr ); 
	} else {
	    fprintf ( stderr, "Change dangles to lines:\n" );
	    Vect_remove_dangles ( &Map, GV_BOUNDARY, -1.0, NULL, stderr ); 
	}

        fprintf ( stderr, separator );
	if ( type & GV_BOUNDARY ) {
	    fprintf ( stderr, "Change boundary bridges to lines:\n" );
	    Vect_chtype_bridges ( &Map, NULL, stderr ); 
	} else {
	    fprintf ( stderr, "Remove bridges:\n" );
	    Vect_remove_bridges ( &Map, NULL, stderr ); 
	}

	/* Boundaries are hopefully clean, build areas */
        fprintf ( stderr, separator );
        Vect_build_partial ( &Map, GV_BUILD_ATTACH_ISLES, stderr );

	/* Calculate new centroids for all areas, centroids have the same id as area */
	ncentr = Vect_get_num_areas ( &Map );
	G_debug (3, "%d centroids/areas", ncentr);

	Centr = (CENTR *) G_calloc ( ncentr+1, sizeof(CENTR) );
	Vect_spatial_index_init ( &si );
	for ( centr = 1; centr <= ncentr; centr++ ) {
	    Centr[centr].valid = 0;
	    Centr[centr].cats = Vect_new_cats_struct ();
	    ret = Vect_get_point_in_area ( &Map, centr, &x, &y );
	    if ( ret < 0 ) {
		G_warning ("Cannot calculate area centroid" );
		continue;
	    }
	    
	    Centr[centr].x = x;
	    Centr[centr].y = y;
	    Centr[centr].valid = 1;
	    box.N = box.S = y;
	    box.E = box.W = x;
	    box.T = box.B = 0;
	    Vect_spatial_index_add_item (&si, centr, &box );
	}

	/* Go through all layers and find centroids for each polygon */
	for ( layer = 0; layer < nlayers; layer++ ) {
	    fprintf (stderr, "Layer: %s\n", layer_names[layer]);
	    layer_id = layers[layer];
	    Ogr_layer = OGR_DS_GetLayer( Ogr_ds, layer_id );
	    OGR_L_ResetReading ( Ogr_layer ); 

	    cat = 0; /* field = layer + 1 */
	    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
		cat++;
		/* Geometry */
		Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
		if ( Ogr_geometry != NULL ) {
		    centroid ( Ogr_geometry, Centr, &si, layer+1, cat, min_area, type );
		} 
	        
		OGR_F_Destroy( Ogr_feature );
	    }
	}

	/* Write centroids */
	n_overlaps = n_nocat = 0;
	total_area = overlap_area = nocat_area = 0.0;
	for ( centr = 1; centr <= ncentr; centr++ ) {
	    double area;

	    area = Vect_get_area_area ( &Map, centr );
	    total_area += area;

	    if ( !(Centr[centr].valid) ) {
		continue;
	    }

	    if ( Centr[centr].cats->n_cats == 0 ) {
		nocat_area += area;
		n_nocat++;
		continue;
	    }
	    
	    if ( Centr[centr].cats->n_cats > 1 ) {
	        Vect_cat_set ( Centr[centr].cats, nlayers+1, Centr[centr].cats->n_cats );
		overlap_area += area;
		n_overlaps++;
	    }

	    Vect_reset_line ( Points );
	    Vect_append_point ( Points, Centr[centr].x, Centr[centr].y, 0.0 );
	    if ( type & GV_POINT ) otype = GV_POINT; else otype = GV_CENTROID;
	    Vect_write_line ( &Map, otype, Points, Centr[centr].cats);
	}
	
        fprintf ( stderr, separator );
	Vect_build_partial (&Map, GV_BUILD_NONE, NULL);

        fprintf ( stderr, separator );
        Vect_build ( &Map, stderr );
        
	fprintf ( stderr, separator );

	if ( n_overlaps > 0 ) {
	    G_warning ("%d areas represent more (overlapping) features, because polygons overlap "
		    "in input layer(s). Such areas are linked to more than 1 row in attribute table. "
		    "The number of features for those areas is stored as category in layer %d.",
		    n_overlaps, nlayers+1 );
	}

	sprintf (buf, "%d input polygons\n", n_polygons); 
	fprintf (stderr, buf );
	Vect_hist_write ( &Map, buf );

	sprintf (buf, "total area: %e (%d areas)\n", total_area, ncentr); 
	fprintf (stderr, buf );
	Vect_hist_write ( &Map, buf );

	sprintf (buf, "overlapping area: %e (%d areas)\n", overlap_area, n_overlaps); 
	fprintf (stderr, buf );
	Vect_hist_write ( &Map, buf );
	
	sprintf (buf, "area without category: %e (%d areas)\n", nocat_area, n_nocat ); 
	fprintf (stderr, buf );
	Vect_hist_write ( &Map, buf );
    }
    
    /* needed?
    * OGR_DS_Destroy( Ogr_ds );
    */

    Vect_close ( &Map );

    if (with_z && !z_flag->answer ) 
	G_warning ( "Input data contains 3D features. Created vector is 2D only, "
		    "use -z flag to import 3D vector.");

    exit(0) ;
}

