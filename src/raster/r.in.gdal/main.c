/*
 * r.in.gdal imports many GIS/image formats into GRASS utilizing the GDAL
 * library
 *
 * copyright of this file
 * Author: Frank Warmerdam
 *
 * Added optional GCP transformation: Markus Neteler 10/2001
 * TODO: most unreferenced formats are read in with negative coordinates - desired??
 */

#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include "gis.h"
#include "imagery.h"
#include "gprojects.h"

#ifdef USE_GDAL_H
#  include "gdal.h"
#  include "ogr_srs_api.h"
#else
#  include "gdalbridge.h"
#endif

#ifndef MAX
#  define MIN(a,b)      ((a<b) ? a : b)
#  define MAX(a,b)      ((a>b) ? a : b)
#endif

static int
wkt_to_grass( const char * wkt, 
              struct Cell_head *cellhd, 
              struct Key_Value **proj_info, 
              struct Key_Value **proj_units );

static void ImportBand( GDALRasterBandH hBand, const char *output,
                        struct Ref *group_ref );
static void SetupReprojector( const char *pszSrcWKT, const char *pszDstLoc,
                              struct pj_info *iproj, 
                              struct pj_info *oproj );

static int l1bdriver;

/************************************************************************/
/*                                main()                                */
/************************************************************************/

int main (int argc, char *argv[])
{
    char *input;
    char *output;
    char *title;
    struct Cell_head cellhd, loc_wind, def_wind;
    struct Key_Value *proj_info=NULL, *proj_units=NULL;
    struct Key_Value *loc_proj_info, *loc_proj_units;
    GDALDatasetH  hDS;
    GDALDriverH   hDriver;
    GDALRasterBandH hBand;
    double          adfGeoTransform[6];
    int         force_imagery = FALSE;
    char	error_msg[8096];
    int 	projcomp_error=0;

	struct GModule *module;
    struct
    {
        struct Option *input, *output, *target, *title, *outloc, *band;
    } parm;
    struct Flag *flag_o, *flag_e, *flag_k;

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
    parm.input->description = "Raster file to be imported";
    parm.input->gisprompt = "file,file,file";

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

    parm.target = G_define_option();
    parm.target->key = "target";
    parm.target->type = TYPE_STRING;
    parm.target->required = NO;
    parm.target->description = "Name of location to read projection from for GCPs transformation";

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

    flag_e = G_define_flag();
    flag_e->key = 'e';
    flag_e->description = "Extend location extents based on new dataset";

    flag_k = G_define_flag();
    flag_k->key = 'k';
    flag_k->description = "Keep band numbers instead of using band color names";

    if (G_parser(argc,argv))
        exit(1);

    input = parm.input->answer;

    output = parm.output->answer;
    if(title = parm.title->answer)
        G_strip (title);

/* -------------------------------------------------------------------- */
/*      Do some additional parameter validation.                        */
/* -------------------------------------------------------------------- */
    if( parm.target->answer && parm.outloc->answer
        && strcmp(parm.target->answer, parm.outloc->answer) == 0 )
    {
        G_fatal_error("You have to specify a target location different from output location");
    }

/* -------------------------------------------------------------------- */
/*      Initialize GDAL Bridge, and open the file.                      */
/* -------------------------------------------------------------------- */
#ifndef USE_GDAL_H
    sprintf( error_msg, "%s/lib", getenv( "GISBASE" ) );                  
    if( !GDALBridgeInitialize( error_msg, stderr ) )
    {
        G_fatal_error( "Unable to initialize GDAL bridge (check libgdal installation/LD_LIBRARY_PATH variable).\n" );
        exit( 10 );
    }
#endif

    GDALAllRegister();
    hDS = GDALOpen( input, GA_ReadOnly );
    if( hDS == NULL )
        return 1;
    hDriver = GDALGetDatasetDriver( hDS ); /* needed for AVHRR data */
    if ( strcmp(GDALGetDriverShortName(hDriver),"L1B") !=0 )
        l1bdriver=0;
    else
        l1bdriver=1; /* AVHRR found, needs north south flip */

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
        if (adfGeoTransform[2] != 0.0 || adfGeoTransform[4] != 0.0)
          G_fatal_error("Input map is rotated - cannot import. You may use 'gdalwarp' to transform the map to North-up.");

        cellhd.north = adfGeoTransform[3];
        cellhd.ns_res = fabs(adfGeoTransform[5]);
        cellhd.south = cellhd.north - cellhd.ns_res * cellhd.rows;
        cellhd.west = adfGeoTransform[0];
        cellhd.ew_res = adfGeoTransform[1];
        cellhd.east = cellhd.west + cellhd.cols * cellhd.ew_res;
    }
    else
    {
        /* use negative XY coordinates per default for unprojected data
	   but not for all formats... (MN: I don't like it at all) */
        /* for hDriver names see gdal/frmts/gdalallregister.cpp */

        if ( l1bdriver || ( strcmp(GDALGetDriverShortName(hDriver),"GTiff") ||  strcmp(GDALGetDriverShortName(hDriver),"JPEG") ) == 0 )
        {
          /* e.g. L1B - NOAA/AVHRR data must be treated differently */
          /* define positive xy coordinate system to avoid GCPs confusion */
          cellhd.north  = cellhd.rows;
          cellhd.south  = 0.0;
          cellhd.ns_res = 1.0;
          cellhd.west   = 0.0;
          cellhd.east   = cellhd.cols;
          cellhd.ew_res = 1.0;
        }
        else
        {
          /* for all other unprojected data ... */
          /* define negative xy coordinate system to avoid GCPs confusion */
          cellhd.north  = 0.0;
          cellhd.south  = (-1) * cellhd.rows;
          cellhd.ns_res = 1.0;
          cellhd.west   = (-1) * cellhd.cols;
          cellhd.east   = 0.0;
          cellhd.ew_res = 1.0;
        }
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
               || (projcomp_error=G_compare_projections( loc_proj_info, loc_proj_units, 
                                          proj_info, proj_units )) < 0 )
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
            strcat( error_msg, "\nERROR: ");
            switch(projcomp_error) {
                  case -1: strcat( error_msg, "proj\n"); break;
                  case -2: strcat( error_msg, "units\n"); break;
                  case -3: strcat( error_msg, "datum\n"); break;
                  case -4: strcat( error_msg, "ellps\n"); break;
                  case -5: strcat( error_msg, "zone\n"); break;
            }
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
        strcat( error_msg, 
         "\nYou can use the -o flag to r.in.gdal to override this check and use the location definition for the dataset.\n" );
        strcat( error_msg, 
         "Consider to generate a new location with 'location' parameter"
         " from input data set.\n" );
        G_fatal_error( error_msg );
    }
    
/* -------------------------------------------------------------------- */
/*      Set the active window to match the available data.              */
/* -------------------------------------------------------------------- */
    if(G_set_window (&cellhd) < 0)
        exit(3);

/* -------------------------------------------------------------------- */
/*      Do we want to generate a simple raster, or an imagery group?    */
/* -------------------------------------------------------------------- */
    if( (GDALGetRasterCount(hDS) > 1 && parm.band->answer == NULL)
        || GDALGetGCPCount( hDS ) > 0 )
        force_imagery = TRUE;

/* -------------------------------------------------------------------- */
/*      Simple case.  Import a single band as a raster cell.            */
/* -------------------------------------------------------------------- */
    if( !force_imagery )
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
        
        ImportBand( hBand, output, NULL );

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
        char    colornamebuf[512], colornamebuf2[512];

        I_init_group_ref( &ref );

        for( nBand = 1; nBand <= GDALGetRasterCount(hDS); nBand++ )
        {
            hBand = GDALGetRasterBand( hDS, nBand );
            hBand = GDALGetRasterBand( hDS, nBand );
            if( !flag_k->answer ){
              /* use channel color names if present: */
              strcpy(colornamebuf,GDALGetColorInterpretationName(
                             GDALGetRasterColorInterpretation(hBand)));

              /* check: two channels with identical name ? */
              if ( strcmp(colornamebuf,colornamebuf2) == 0 )
                   sprintf(colornamebuf,"%d",nBand);
              else
                   strcpy(colornamebuf2,colornamebuf);

              /* avoid bad color names; in case of 'Gray' often all channels are named 'Gray' */
              if ( strcmp(colornamebuf,"Undefined") == 0 || strcmp(colornamebuf,"Gray") == 0 )
                   sprintf( szBandName, "%s.%d", output, nBand);
              else
              {
                   G_tolcase(colornamebuf);
                   sprintf( szBandName, "%s.%s", output, colornamebuf);
              }
            } else
	        sprintf( szBandName, "%s.%d", output, nBand);

            ImportBand( hBand, szBandName, &ref );
            if (title)
                G_put_cell_title (szBandName, title);
        }

        I_put_group_ref( output, &ref );
        I_free_group_ref( &ref );

        /* make this group the current group */
        I_put_group( output );

/* -------------------------------------------------------------------- */
/*      Output GCPs if present, we can only do this when writing an     */
/*      imagery group.                                                  */
/* -------------------------------------------------------------------- */
        if( GDALGetGCPCount( hDS ) > 0 )
        {
            struct Control_Points sPoints;
            const GDAL_GCP *pasGCPs = GDALGetGCPs( hDS );
            int iGCP;
            struct pj_info iproj,            /* input map proj parameters    */
                      oproj;                 /* output map proj parameters   */

            sPoints.count = GDALGetGCPCount( hDS );
            sPoints.e1 = (double *) malloc(sizeof(double) * sPoints.count * 4);
            sPoints.n1 = sPoints.e1 + sPoints.count;
            sPoints.e2 = sPoints.e1 + 2 * sPoints.count;
            sPoints.n2 = sPoints.e1 + 3 * sPoints.count;
            sPoints.status = (int *) malloc(sizeof(int) * sPoints.count);
            
            fprintf (stderr, "COPYING %d GCPS IN POINTS FILE FOR %s\n", 
                     sPoints.count, output );
            if( GDALGetGCPProjection(hDS) != NULL 
                && strlen(GDALGetGCPProjection(hDS)) > 0 )
            {
                fprintf(stderr, 
                    "\n"
                    "GCPs have the following OpenGIS WKT Coordinate System:\n"
                    "%s\n", 
                    GDALGetGCPProjection( hDS ) );
            }

            if (parm.target->answer)
            {
                SetupReprojector( GDALGetGCPProjection(hDS), 
                                  parm.target->answer, 
                                  &iproj, &oproj );
                fprintf(stderr, "Re-projecting GCPs table:\n");
                fprintf(stderr, " Input projection for GCP table:  %s\n", 
                        iproj.proj);
                fprintf(stderr, " Output projection for GCP table: %s\n", 
                        oproj.proj);
            }

            for( iGCP = 0; iGCP < sPoints.count; iGCP++ )
            {
                if ( !l1bdriver)
                {
                  sPoints.e1[iGCP] = (-1) * pasGCPs[iGCP].dfGCPPixel; /* neg. xy */
                  sPoints.n1[iGCP] = (-1) * pasGCPs[iGCP].dfGCPLine;
                }
                else /* L1B - NOAA/AVHRR */
                {
                  sPoints.e1[iGCP] = pasGCPs[iGCP].dfGCPPixel;    /* pos. xy */
                  sPoints.n1[iGCP] = pasGCPs[iGCP].dfGCPLine;
                }
                
                sPoints.e2[iGCP] = pasGCPs[iGCP].dfGCPX;          /* target */
                sPoints.n2[iGCP] = pasGCPs[iGCP].dfGCPY;
                sPoints.status[iGCP] = 1;

                /* If desired, do GCPs transformation to other projection */
                if (parm.target->answer)
                {
                    /* re-project target GCPs */
                    if(pj_do_proj( &(sPoints.e2[iGCP]), &(sPoints.n2[iGCP]),
                                   &iproj, &oproj) < 0)
                        G_fatal_error("Error in pj_do_proj (can't "
                                      "re-projection GCP %i)\n", 
                                      iGCP);
                }
            } /* for all GCPs*/

            I_put_control_points( output, &sPoints );

            free( sPoints.e1 );
            free( sPoints.status );
        }
    }

/* -------------------------------------------------------------------- */
/*      Extend current window based on dataset.                         */
/* -------------------------------------------------------------------- */
    if( flag_e->answer )
    {
        G_get_default_window( &def_wind );

        def_wind.north = MAX(def_wind.north,cellhd.north);
        def_wind.south = MIN(def_wind.south,cellhd.south);
        def_wind.west  = MIN(def_wind.west, cellhd.west);
        def_wind.east  = MAX(def_wind.east, cellhd.east);

        def_wind.rows = (int) ceil((def_wind.north - def_wind.south) 
                                   / def_wind.ns_res);
        def_wind.south = def_wind.north - def_wind.rows * def_wind.ns_res;
        
        def_wind.cols = (int) ceil((def_wind.east - def_wind.west) 
                                   / def_wind.ew_res);
        def_wind.east = def_wind.west + def_wind.cols * def_wind.ew_res;
        
        G__put_window( &def_wind, "../PERMANENT", "DEFAULT_WIND" );
    } 

    exit (0);
}

/************************************************************************/
/*                          SetupReprojector()                          */
/************************************************************************/

static void SetupReprojector( const char *pszSrcWKT, const char *pszDstLoc,
                              struct pj_info *iproj, 
                              struct pj_info *oproj )

{
    struct Cell_head cellhd;
    struct Key_Value *proj_info=NULL, *proj_units=NULL;
    char errbuf[256];
    int permissions;
    char target_mapset[80];
    struct Key_Value *out_proj_info,  /* projection information of    */
                     *out_unit_info;  /* input and output mapsets     */

/* -------------------------------------------------------------------- */
/*      Translate GCP WKT coordinate system into GRASS format.          */
/* -------------------------------------------------------------------- */
    wkt_to_grass( pszSrcWKT, &cellhd, &proj_info, &proj_units );

    if (pj_get_kv(iproj, proj_info, proj_units) < 0)
        G_fatal_error("Can't translate projection key values of input GCPs.");

/* -------------------------------------------------------------------- */
/*      Get the projection of the target location.                      */
/* -------------------------------------------------------------------- */

    /* Change to user defined target location for GCPs transformation */
    G__create_alt_env();
    G__setenv("LOCATION_NAME", (char *) pszDstLoc);
    sprintf(target_mapset, "PERMANENT"); /* to find PROJ_INFO */

    permissions = G__mapset_permissions(target_mapset);
    if (permissions >= 0) {

        /* Get projection info from target location */
        if ((out_proj_info = G_get_projinfo()) == NULL)
            G_fatal_error("Can't get projection info of target location");
        if ((out_unit_info = G_get_projunits()) == NULL)
            G_fatal_error("Can't get projection units of target location");
        if (pj_get_kv(oproj, out_proj_info, out_unit_info) < 0)
           G_fatal_error("Can't get projection key values of target location");
    }
    else
    { /* can't access target mapset */
        sprintf(errbuf, "Mapset [%s] in target location [%s] - ",
                target_mapset, pszDstLoc);
        strcat(errbuf, permissions == 0
               ? "permission denied\n"
               : "not found\n");
        G_fatal_error(errbuf);
    } /* permission check */
                
    /* And switch back to original location */
    G__switch_env();
}


/************************************************************************/
/*                             ImportBand()                             */
/************************************************************************/

static void ImportBand( GDALRasterBandH hBand, const char *output,
                        struct Ref *group_ref )

{
    RASTER_MAP_TYPE data_type;
    GDALDataType    eGDT, eRawGDT;
    int row, nrows, ncols, complex;
    int cf, cfR, cfI, bNoDataEnabled;
    int indx;
    CELL *cell,*cellReal,*cellImg;
    float *bufComplex;
    double dfNoData;
    char msg[100];
    char outputReal[200], outputImg[200];
    char *nullFlags = NULL;
    int (*raster_open_new_func)(char *, RASTER_MAP_TYPE) = G_open_raster_new;

/* -------------------------------------------------------------------- */
/*      Select a cell type for the new cell.                            */
/* -------------------------------------------------------------------- */
    eRawGDT = GDALGetRasterDataType( hBand );
    
    switch(eRawGDT) {
      case GDT_Float32:
      case GDT_Float64:
        data_type = FCELL_TYPE;
        eGDT = GDT_Float32;
        complex = FALSE;
        break;

      case GDT_Byte:
        data_type = CELL_TYPE;
        eGDT = GDT_Int32;
        complex = FALSE;
        G_set_cell_format(0);
        /* raster_open_new_func = G_open_raster_new_uncompressed;*/ /* ?? */
        break;

      case GDT_Int16:
      case GDT_UInt16:
        data_type = CELL_TYPE;
        eGDT = GDT_Int32;
        complex = FALSE;
        G_set_cell_format(1);
        /* raster_open_new_func = G_open_raster_new_uncompressed;*/ /* ?? */
        break;

      default:
        data_type = CELL_TYPE;
        eGDT = GDT_Int32;
        complex = FALSE;
	G_set_cell_format(3);
        break;
    }
    
/* -------------------------------------------------------------------- */
/*      Create the new raster(s)                                          */
/* -------------------------------------------------------------------- */
    ncols = GDALGetRasterBandXSize(hBand);
    nrows = GDALGetRasterBandYSize(hBand);

    if( complex )
    {
        sprintf( outputReal, "%s.real", output);
        cfR = (*raster_open_new_func)((char *)outputReal, data_type);
        if (cfR < 0)
	{
            sprintf (msg, "unable to create raster map %s", outputReal);
            G_fatal_error (msg);
            exit(1);
	}
        sprintf( outputImg, "%s.imaginary", output);

        cfI = (*raster_open_new_func)((char *)outputImg, data_type);
        if (cfI < 0)
	{
            sprintf (msg, "unable to create raster map %s", outputImg);
            G_fatal_error (msg);
            exit(1);
	}

        cellReal = G_allocate_raster_buf(data_type);
        cellImg = G_allocate_raster_buf(data_type);
        bufComplex = (float *) G_malloc(sizeof(float) * ncols * 2);

        if( group_ref != NULL )
        {
            I_add_file_to_group_ref (outputReal, G_mapset(), group_ref);
            I_add_file_to_group_ref (outputImg, G_mapset(), group_ref);
        }
    }
    else
    {
        cf = (*raster_open_new_func)((char *)output, data_type);
        if (cf < 0)
	{
            sprintf (msg, "unable to create raster map %s", output);
            G_fatal_error (msg);
            exit(1);
	}

        if( group_ref != NULL )
            I_add_file_to_group_ref ((char *) output, G_mapset(), group_ref);

        cell = G_allocate_raster_buf(data_type);
    }

/* -------------------------------------------------------------------- */
/*      Do we have a null value?                                        */
/* -------------------------------------------------------------------- */
    dfNoData = GDALGetRasterNoDataValue( hBand, &bNoDataEnabled );
    if( bNoDataEnabled )
    {
        nullFlags = (char *) G_malloc(sizeof(char) * ncols);
        memset( nullFlags, 0, ncols );
    }

/* -------------------------------------------------------------------- */
/*      Write the raster one scanline at a time.                        */
/*      We have to distinguish some cases due to the different          */
/*      coordinate system orientation of GDAL and GRASS for xy data     */
/* -------------------------------------------------------------------- */
   if ( !l1bdriver )
   { /* no AVHRR */
    for (row = 1; row <= nrows; row++)
    {
        if( complex )  /* CEOS SAR et al.: import flipped to match GRASS coordinates */
        {
            GDALRasterIO( hBand, GF_Read, 0, row-1, ncols, 1, 
                          bufComplex, ncols, 1, eGDT, 0, 0 );
            
            for( indx=ncols-1; indx >= 0; indx-- ) /* CEOS: flip east-west during import - MN */
            {
                if( eGDT == GDT_Int32 )
                {
                    ((GInt32 *) cellReal)[ncols-indx] = 
                        ((GInt32 *) bufComplex)[indx*2];
                    ((GInt32 *) cellImg)[ncols-indx]  = 
                        ((GInt32 *) bufComplex)[indx*2+1];
                }
                else
                {
                    ((float *) cellReal)[ncols-indx] = bufComplex[indx*2];
                    ((float *) cellImg)[ncols-indx]  = bufComplex[indx*2+1];
                }
            }
            G_put_raster_row (cfR, cellReal, data_type);
            G_put_raster_row (cfI, cellImg, data_type);
        } /* end of complex */
        else
        {   /* single band */
            GDALRasterIO( hBand, GF_Read, 0, row-1, ncols, 1, 
                          cell, ncols, 1, eGDT, 0, 0 );

            if( nullFlags != NULL )
            {
                memset( nullFlags, 0, ncols );

                if( eGDT == GDT_Int32 )
                {
                    for( indx=0; indx < ncols; indx++ ) 
                    {
                        if( ((GInt32 *) cell)[indx] == (GInt32) dfNoData )
                        {
                            nullFlags[indx] = 1;
                        }
                    }
                }
                else if( eGDT == GDT_Float32 )
                {
                    for( indx=0; indx < ncols; indx++ ) 
                    {
                        if( ((float *) cell)[indx] == (float) dfNoData )
                        {
                            nullFlags[indx] = 1;
                        }
                    }
                }

                G_insert_null_values( cell, nullFlags, ncols, data_type);
            }
            
            G_put_raster_row (cf, cell, data_type);
        } /* end of not complex */

        G_percent(row, nrows, 2);
    }	/* for loop */
   } /* end of not AVHRR */
   else
   {
    /* AVHRR - read from south to north to match GCPs*/
    for (row = nrows; row > 0; row--)
    {
            GDALRasterIO( hBand, GF_Read, 0, row-1, ncols, 1, 
                          cell, ncols, 1, eGDT, 0, 0 );

            if( nullFlags != NULL )
            {
                memset( nullFlags, 0, ncols );

                if( eGDT == GDT_Int32 )
                {
                    for( indx=0; indx < ncols; indx++ ) 
                    {
                        if( ((GInt32 *) cell)[indx] == (GInt32) dfNoData )
                        {
                            nullFlags[indx] = 1;
                        }
                    }
                }
                else if( eGDT == GDT_Float32 )
                {
                    for( indx=0; indx < ncols; indx++ ) 
                    {
                        if( ((float *) cell)[indx] == (float) dfNoData )
                        {
                            nullFlags[indx] = 1;
                        }
                    }
                }

                G_insert_null_values( cell, nullFlags, ncols, data_type);
            }
            
            G_put_raster_row (cf, cell, data_type);
        }

        G_percent(row, nrows, 2);
    } /* end AVHRR */
/* -------------------------------------------------------------------- */
/*      Cleanup                                                         */
/* -------------------------------------------------------------------- */
    if( complex ) 
    {
        fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", outputReal);
        G_close_cell (cfR);

        fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", outputImg);
        G_close_cell (cfI);

        G_free( bufComplex );
    }
    else
    {
        fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", output);
        G_close_cell (cf);
    }

    if( nullFlags != NULL )
        G_free( nullFlags );

/* -------------------------------------------------------------------- */
/*      Transfer colormap, if there is one.                             */
/* -------------------------------------------------------------------- */
    if( !complex && GDALGetRasterColorTable( hBand ) != NULL )
    {
        GDALColorTableH  hCT;
        struct Colors    colors;
        int              iColor;

        fprintf (stderr, "COPYING COLOR TABLE FOR %s\n", output );

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
    else /* no color table present */
    {
        /* types are defined in GDAL: ./core/gdal.h */
        if ( (GDALGetRasterDataType(hBand) == GDT_Byte) )
        {
           /* found 0..255 data: we set to grey scale: */
           struct Colors    colors;
        
           fprintf (stderr, "SETTING GREY COLOR TABLE FOR %s (8bit, full range)\n", output );

           G_init_colors (&colors);
           G_make_grey_scale_colors (&colors, 0, 255); /* full range */
           G_write_colors( (char *) output, G_mapset(), &colors);
        }
        if ( (GDALGetRasterDataType(hBand) == GDT_UInt16) )
        {
           /* found 0..65535 data: we set to grey scale: */
           struct Colors    colors;
           struct Range     range;
           CELL             min, max;
        
           fprintf (stderr, "SETTING GREY COLOR TABLE FOR %s (16bit, image range)\n", output );
           G_read_range( (char *) output, G_mapset(), &range) ;
           G_get_range_min_max (&range, &min, &max);

           G_init_colors (&colors);
           G_make_grey_scale_colors (&colors, min, max); /* image range */
           G_write_colors( (char *) output, G_mapset(), &colors);
        }
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
    char *pszProj = NULL;

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
            pszValue = "defined";
        
        /* The latlong projection is known as ll in GRASS */
        if( G_strcasecmp(pszToken,"proj") == 0
            && G_strcasecmp(pszValue,"longlat") == 0 )
            pszValue = "ll";

        if( G_strcasecmp(pszToken,"proj") == 0 )
            pszProj = pszValue;

        /* All the ellipsoid keys in GRASS are lower case.  Eventually we
           may need to remove this hack.  See bug 1047 in RT bug tracker. */
        if( G_strcasecmp(pszToken,"ellps") == 0 )
            G_tolcase( pszValue );

        /* We will handle units separately */
        if( G_strcasecmp(pszToken,"to_meter") == 0 
            || G_strcasecmp(pszToken,"units") == 0 )
            continue;

        G_set_key_value( pszToken, pszValue, *proj_info );
    }

/* -------------------------------------------------------------------- */
/*      Derive the user name for the projection.                        */
/* -------------------------------------------------------------------- */
    {
        char	path[4095];
        char    name[80];

        sprintf(path,"%s/etc/projections",G_gisbase());
        if( G_lookup_key_value_from_file(path,pszProj,name,sizeof(name)) > 0 )
            G_set_key_value( "name", name, *proj_info );
        else
            G_set_key_value( "name", pszProj, *proj_info );
    }

/* -------------------------------------------------------------------- */
/*	Despite having the +ellps set, GRASS still requires +a and +es	*/
/* -------------------------------------------------------------------- */
    {
        const char *pszSemiMajor = OSRGetAttrValue( hSRS, "SPHEROID", 1 );
        const char *pszInvFlat = OSRGetAttrValue( hSRS, "SPHEROID", 2 );

        if( strstr(pszProj4,"+a") == NULL && pszSemiMajor != NULL )
            G_set_key_value( "a", (char *) pszSemiMajor, *proj_info );

        if( pszInvFlat != NULL )
        {
            double	es, flat;
            char	es_str[100];

            flat = 1 / atof(pszInvFlat);
            
            es = flat * (2.0 - flat);

            sprintf( es_str, "%.10f", es );
            G_set_key_value( "es", es_str, *proj_info );
        }
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
        G_set_key_value( "meters", "1.0", *proj_units );
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
        G_set_key_value( "meters", szFormatBuf, *proj_units );

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

