#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include "gis.h"
#include "imagery.h"
#include "gdalbridge.h"

static int oldval = 0, newval = 0;

static int
wkt_to_grass( const char * wkt, 
              struct Cell_head *cellhd, 
              struct Key_Value **proj_info, 
              struct Key_Value **proj_units );

static int 
G_compare_projections( struct Key_Value *proj_info1, 
                       struct Key_Value *proj_units1, 
                       struct Key_Value *proj_info2, 
                       struct Key_Value *proj_units2 );

static void ImportBand( GDALRasterBandH hBand, const char *output );

/************************************************************************/
/*                                main()                                */
/************************************************************************/

int main (int argc, char *argv[])
{
    char *input;
    char *output;
    char *title;
    struct Cell_head cellhd, loc_wind;
    struct Key_Value *proj_info, *proj_units;
    struct Key_Value *loc_proj_info, *loc_proj_units;
    unsigned char *x, *y;
    char *err;
    char dummy[2];
    GDALDatasetH  hDS;
    GDALRasterBandH hBand;
    double          adfGeoTransform[6];
    char	error_msg[8096];

	struct GModule *module;
    struct
    {
        struct Option *input, *output, *title, *outloc, *band;
    } parm;
    struct Flag *flag_o;

/* -------------------------------------------------------------------- */
/*      Initialize.                                                     */
/* -------------------------------------------------------------------- */
    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Import GDAL supported raster file into a binary raster map layer.";

/* -------------------------------------------------------------------- */
/*      Setup and fetch parameters.                                     */
/* -------------------------------------------------------------------- */
    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "Bin raster file to be imported";

    parm.band = G_define_option();
    parm.band->key = "band";
    parm.band->type = TYPE_INTEGER;
    parm.band->required = NO;
    parm.band->description = "Band to select (default is all bands)";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->description = "Name for resultant raster map";
    parm.output->gisprompt = "any,cell,raster";

    parm.title = G_define_option();
    parm.title->key = "title";
    parm.title->key_desc = "\"phrase\"";
    parm.title->type = TYPE_STRING;
    parm.title->required = NO;
    parm.title->description = "Title for resultant raster map";

    parm.outloc = G_define_option();
    parm.outloc->key = "location";
    parm.outloc->type = TYPE_STRING;
    parm.outloc->required = NO;
    parm.outloc->description = "Name for new location to create";

    flag_o = G_define_flag();
    flag_o->key = 'o';
    flag_o->description = "Override projection (use locations projection)";

    if (G_parser(argc,argv))
        exit(1);

    input = parm.input->answer;

    output = parm.output->answer;
    if(title = parm.title->answer)
        G_strip (title);

/* -------------------------------------------------------------------- */
/*      Initialize GDAL Bridge, and open the file.                      */
/* -------------------------------------------------------------------- */
    if( !GDALBridgeInitialize( "/home/warmerda/gdal" ) )
    {
        G_fatal_error( "Unable to initialize GDAL bridge.\n" );
        exit( 10 );
    }

    GDALAllRegister();
    hDS = GDALOpen( input, GA_ReadOnly );
    if( hDS == NULL )
        return 1;
    
/* -------------------------------------------------------------------- */
/*      Fetch the projection in GRASS form.                             */
/* -------------------------------------------------------------------- */
    proj_info = NULL;
    proj_units = NULL;
    wkt_to_grass( GDALGetProjectionRef(hDS), &cellhd, &proj_info, &proj_units);

/* -------------------------------------------------------------------- */
/*      Set up the window representing the data we have.                */
/* -------------------------------------------------------------------- */
    cellhd.rows = GDALGetRasterYSize(hDS);
    cellhd.cols = GDALGetRasterXSize(hDS);

    if( GDALGetGeoTransform( hDS, adfGeoTransform ) == CE_None
        && adfGeoTransform[5] < 0.0 )
    {
        cellhd.north = adfGeoTransform[3];
        cellhd.ns_res = fabs(adfGeoTransform[5]);
        cellhd.south = cellhd.north - cellhd.ns_res * cellhd.rows;
        cellhd.west = adfGeoTransform[0];
        cellhd.ew_res = adfGeoTransform[1];
        cellhd.east = cellhd.west + cellhd.cols * cellhd.ew_res;
    }
    else
    {
        cellhd.north = cellhd.rows;
        cellhd.south = 0.0;
        cellhd.ns_res = 1.0;
        cellhd.west = 0.0;
        cellhd.east = cellhd.cols;
        cellhd.ew_res = 1.0;
    }

/* -------------------------------------------------------------------- */
/*      Do we need to create a new location?                            */
/* -------------------------------------------------------------------- */
    if( parm.outloc->answer != NULL )
    {
        G_make_location( parm.outloc->answer, &cellhd, 
                         proj_info, proj_units, NULL );
    }

/* -------------------------------------------------------------------- */
/*      Does the projection of the current location match the           */
/*      dataset?                                                        */
/* -------------------------------------------------------------------- */
    loc_proj_info = G_get_projinfo();
    loc_proj_units = G_get_projunits();
    G_get_window( &loc_wind );

    if( flag_o->answer )
    {
        cellhd.proj = loc_wind.proj;
        cellhd.zone = loc_wind.zone;
    } 
    else if( loc_wind.proj != cellhd.proj
               || !G_compare_projections( loc_proj_info, loc_proj_units, 
                                          proj_info, proj_units ) )
    {
        int     i_value;

        strcpy( error_msg, 
                "Projection of dataset does not"
                " appear to match current location.\n\n");

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
        G_fatal_error( error_msg );
    }
    
/* -------------------------------------------------------------------- */
/*      Set the active window to match the available data.              */
/* -------------------------------------------------------------------- */
    if(G_set_window (&cellhd) < 0)
        exit(3);

/* -------------------------------------------------------------------- */
/*      Simple case.  Import a single band as a raster cell.            */
/* -------------------------------------------------------------------- */
    if( parm.band->answer != NULL || GDALGetRasterCount(hDS) == 1 )
    {
        int	nBand = 1;
        
        if( parm.band->answer != NULL )
            nBand = atoi(parm.band->answer);
        
        hBand = GDALGetRasterBand(hDS,1);
        if( hBand == NULL )
        {
            sprintf( error_msg, "Selected band (%d) does not exist.\n", 
                     nBand );
            G_fatal_error( error_msg );
        }
        
        ImportBand( hBand, output );

        if (title)
            G_put_cell_title (output, title);
    }

/* -------------------------------------------------------------------- */
/*      Complete case, import a set of raster bands as an imagery       */
/*      group.                                                          */
/* -------------------------------------------------------------------- */
    else 
    {
        struct Ref ref;
        char	szBandName[512];
        int     nBand;

        I_init_group_ref( &ref );

        for( nBand = 1; nBand <= GDALGetRasterCount(hDS); nBand++ )
        {
            hBand = GDALGetRasterBand(hDS,nBand);

            sprintf( szBandName, "%s.%d", output, nBand );
            ImportBand( hBand, szBandName );
            I_add_file_to_group_ref (szBandName, G_mapset(), &ref);

            if (title)
                G_put_cell_title (szBandName, title);
        }

        I_put_group_ref( output, &ref );
        I_free_group_ref( &ref );

        /* make this group the current group */
        I_put_group( output );
    }

    exit (0);
}

/************************************************************************/
/*                             ImportBand()                             */
/************************************************************************/

static void ImportBand( GDALRasterBandH hBand, const char *output )

{
    RASTER_MAP_TYPE data_type;
    GDALDataType    eGDT;
    int row, col, nrows, ncols;
    int cf;
    CELL *cell;

/* -------------------------------------------------------------------- */
/*      Select a cell type for the new cell.                            */
/* -------------------------------------------------------------------- */
    if( GDALGetRasterDataType( hBand ) == GDT_Float32
        || GDALGetRasterDataType( hBand ) == GDT_Float64 )
    {
        data_type = FCELL_TYPE;
        eGDT = GDT_Float32;
    }
    else
    {
        data_type = CELL_TYPE;
        eGDT = GDT_Int32;
    }
    
/* -------------------------------------------------------------------- */
/*      Create the new raster                                           */
/* -------------------------------------------------------------------- */
    cf = G_open_raster_new((char *)output, data_type);
    if (cf < 0)
    {
        char msg[100];
        sprintf (msg, "unable to create raster map %s", output);
        G_fatal_error (msg);
        exit(1);
    }

/* -------------------------------------------------------------------- */
/*      Write the raster one scanline at a time.                        */
/* -------------------------------------------------------------------- */
    ncols = GDALGetRasterBandXSize(hBand);
    nrows = GDALGetRasterBandYSize(hBand);

    cell = G_allocate_raster_buf(data_type);

    for (row = 1; row <= nrows; row++)
    {
        GDALRasterIO( hBand, GF_Read, 0, row-1, ncols, 1, 
                      cell, ncols, 1, eGDT, 0, 0 );

        G_percent(row, nrows, 2);
        G_put_raster_row (cf, cell, data_type);
    }

/* -------------------------------------------------------------------- */
/*      Cleanup                                                         */
/* -------------------------------------------------------------------- */
    fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", output);
    G_close_cell (cf);

/* -------------------------------------------------------------------- */
/*      Transfer colormap, if there is one.                             */
/* -------------------------------------------------------------------- */
    if( GDALGetRasterColorTable( hBand ) != NULL )
    {
        GDALColorTableH  hCT;
        struct Colors    colors;
        int              iColor;

        hCT = GDALGetRasterColorTable( hBand );
        
        G_init_colors (&colors);
        for (iColor = 0; iColor < GDALGetColorEntryCount( hCT ); iColor++ )
        {
            GDALColorEntry  sEntry;

            GDALGetColorEntryAsRGB( hCT, iColor, &sEntry );
            if( sEntry.c4 == 0 )
                continue;

            G_set_color(iColor, sEntry.c1, sEntry.c2, sEntry.c3, &colors);
        }

        G_write_colors( (char *) output, G_mapset(), &colors);
    }
}

/************************************************************************/
/*                            wkt_to_grass()                            */
/************************************************************************/

static int 
wkt_to_grass( const char * wkt, 
              struct Cell_head *cellhd, 
              struct Key_Value **proj_info, 
              struct Key_Value **proj_units )

{
    OGRSpatialReferenceH  hSRS = NULL;
    char *pszProj4 = NULL, *pszRemaining;

    hSRS = OSRNewSpatialReference(NULL);
    
    if( OSRImportFromWkt( hSRS, (char **) &wkt ) != OGRERR_NONE )
        goto default_to_xy;

/* -------------------------------------------------------------------- */
/*      Set cellhd for well known coordinate systems.                   */
/* -------------------------------------------------------------------- */
    if( !OSRIsGeographic( hSRS ) && !OSRIsProjected( hSRS ) )
        goto default_to_xy;

    if( cellhd )
    {
        int   bNorth;

        if( OSRIsGeographic( hSRS ) )
        {
            cellhd->proj = PROJECTION_LL;
            cellhd->zone = 0;
        }
        else if( OSRGetUTMZone( hSRS, &bNorth ) != 0 )
        {
            cellhd->proj = PROJECTION_UTM;
            cellhd->zone = OSRGetUTMZone( hSRS, &bNorth );
            if( !bNorth )
                cellhd->zone *= -1;
        }
        else 
        {
            cellhd->proj = PROJECTION_OTHER;
            cellhd->zone = 0;
        }
    }

/* -------------------------------------------------------------------- */
/*      Get the coordinate system definition in PROJ.4 format.          */
/* -------------------------------------------------------------------- */
    if( OSRExportToProj4( hSRS, &pszProj4 ) != OGRERR_NONE )
        goto default_to_xy;

/* -------------------------------------------------------------------- */
/*      Parse the PROJ.4 string into key/value pairs.  Do a bit of      */
/*      extra work to "GRASSify" the result.                            */
/* -------------------------------------------------------------------- */
    *proj_info = G_create_key_value();
    pszRemaining = pszProj4;
    while( (pszRemaining = strstr(pszRemaining,"+")) != NULL )
    {
        char 	*pszToken, *pszValue;

        pszRemaining++;
        
        /* Advance pszRemaining to end of this token[=value] pair */
        pszToken = pszRemaining;
        while( *pszRemaining != ' ' && *pszRemaining != '\0' )
            pszRemaining++;

        if( *pszRemaining == ' ' )
        {
            *pszRemaining = '\0';
            pszRemaining++;
        }

        /* parse token, and value */
        if( strstr(pszToken,"=") != NULL )
        {
            pszValue = strstr(pszToken,"=");
            *pszValue = '\0';
            pszValue++;
        }
        else
            pszValue = "";
        
        /* The latlong projection is known as ll in GRASS */
        if( G_strcasecmp(pszToken,"proj") == 0
            && G_strcasecmp(pszValue,"longlat") == 0 )
            pszValue = "ll";

        /* We will handle units separately */
        if( G_strcasecmp(pszToken,"to_meter") == 0 
            || G_strcasecmp(pszToken,"units") == 0 )
            continue;

        G_set_key_value( pszToken, pszValue, *proj_info );
    }

    free( pszProj4 ); /* hopefully the same as CPLFree()! */

/* -------------------------------------------------------------------- */
/*      Set the linear units.                                           */
/* -------------------------------------------------------------------- */
    *proj_units = G_create_key_value();
    
    if( OSRIsGeographic( hSRS ) )
    {
        /* We assume degrees ... someday we will be wrong! */
        G_set_key_value( "unit", "degree", *proj_units );
        G_set_key_value( "units", "degrees", *proj_units );
        G_set_key_value( "meter", "1.0", *proj_units );
    }
    else 
    {
        char	szFormatBuf[256];
        char    *pszUnitsName = NULL;
        double  dfToMeters;

        dfToMeters = OSRGetLinearUnits( hSRS, &pszUnitsName );
        
        G_set_key_value( "unit", pszUnitsName, *proj_units );
        sprintf( szFormatBuf, "%ss", pszUnitsName );
        G_set_key_value( "units", szFormatBuf, *proj_units );
        sprintf( szFormatBuf, "%g", dfToMeters );
        G_set_key_value( "meter", szFormatBuf, *proj_units );

        free( pszUnitsName );
    }

    return TRUE;

/* -------------------------------------------------------------------- */
/*      Fallback to returning an ungeoreferenced definition.            */
/* -------------------------------------------------------------------- */
  default_to_xy:
    if( hSRS != NULL )
        OSRDestroySpatialReference( hSRS );

    if( cellhd != NULL )
    {
        cellhd->proj = PROJECTION_XY;
        cellhd->zone = 0;
    }

    *proj_info = NULL;
    *proj_units = NULL;
    
    return FALSE; /* error */
}

/************************************************************************/
/*                       G_compare_projections()                        */
/************************************************************************/

static int 
G_compare_projections( struct Key_Value *proj_info1, 
                       struct Key_Value *proj_units1, 
                       struct Key_Value *proj_info2, 
                       struct Key_Value *proj_units2 )

{
    if( proj_info1 == NULL && proj_info2 == NULL )
        return TRUE;
    
/* -------------------------------------------------------------------- */
/*      Verify that the linear unit translation to meters is OK.        */
/* -------------------------------------------------------------------- */
    if( proj_units1 != NULL && proj_units2 != NULL
        && G_find_key_value( "meter", proj_units1 ) != NULL
        && G_find_key_value( "meter", proj_units1 ) != NULL
        && atof(G_find_key_value( "meter", proj_units1 ))
           != atof(G_find_key_value( "meter", proj_units2 )) )
        return FALSE;

/* -------------------------------------------------------------------- */
/*      Are they both in the same projection?                           */
/* -------------------------------------------------------------------- */
    if( G_find_key_value( "proj", proj_units1 ) != NULL
        && G_find_key_value( "meter", proj_units1 ) != NULL
        && atof(G_find_key_value( "meter", proj_units1 ))
           != atof(G_find_key_value( "meter", proj_units2 )) )
        return FALSE;

/* -------------------------------------------------------------------- */
/*      Add more details in later.                                      */
/* -------------------------------------------------------------------- */


    return TRUE;
}
