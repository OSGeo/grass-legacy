/*  
 ****************************************************************************
 *
 * MODULE:       g.proj 
 * AUTHOR(S):    Paul Kelly - paul-grass@stjohnspoint.co.uk
 * PURPOSE:      Provides a means of reporting the contents of GRASS
 *               projection information files and creating
 *               new projection information files.
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include "config.h"
#include <stdio.h>
#include "gis.h"
#include "gprojects.h"
#ifdef HAVE_OGR
#  include <gdal.h>
#  include <ogr_api.h>
#  include <cpl_csv.h>
#endif

int main(int argc, char *argv[])
{
    struct Flag *printinfo,	/* Print contents of PROJ_INFO & PROJ_UNITS */
        *printproj4,		/* Print projection in PROJ.4 format        */
        *datuminfo,		/* Check if datum information is present    */
        *create,		/* Create new projection files              */
#ifdef HAVE_OGR
        *printwkt,		/* Print projection in WKT format           */
        *esristyle,		/* Use ESRI-style WKT format                */
#endif
        *dontprettify;		/* Print 'flat' output (no linebreaks)      */

    struct Option *location	/* Name of new location to create           */
#ifdef HAVE_OGR
          ,
          *inwkt,	        /* Input file with projection in WKT format */
          *inproj4,		/* Projection in PROJ.4 format              */
          *ingeo;		/* Input geo-referenced file readable by 
				 * GDAL or OGR                              */
          int importformats
#endif
          ;
   
    struct Key_Value *projinfo = NULL, *projunits = NULL;
    struct Key_Value *old_projinfo = NULL, *old_projunits = NULL;
    struct Cell_head cellhd, old_cellhd;
    struct GModule *module;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	"Prints and manipulates GRASS projection information files.";

    printinfo = G_define_flag();
    printinfo->key = 'p';
    printinfo->description = "Print projection information (in conventional "
	"GRASS format)";

    datuminfo = G_define_flag();
    datuminfo->key = 'd';
    datuminfo->description = "Verify datum information and print "
	"transformation parameters";

    printproj4 = G_define_flag();
    printproj4->key = 'j';
    printproj4->description = "Print projection information "
	"in PROJ.4 format";

#ifdef HAVE_OGR
    printwkt = G_define_flag();
    printwkt->key = 'w';
    printwkt->description = "Print projection information in WKT format";

    esristyle = G_define_flag();
    esristyle->key = 'e';
    esristyle->description =
	"Use ESRI-style format (applies to WKT output only)";

#endif

    dontprettify = G_define_flag();
    dontprettify->key = 'f';
    dontprettify->description = "Print 'flat' output with no linebreaks "
	"(applies to WKT and PROJ.4 output)";

#ifdef HAVE_OGR
    ingeo = G_define_option();
    ingeo->key = "georef";
    ingeo->type = TYPE_STRING;
    ingeo->required = NO;
    ingeo->description = "Georeferenced data file to read projection "
	"information from";

    inwkt = G_define_option();
    inwkt->key = "wkt";
    inwkt->type = TYPE_STRING;
    inwkt->required = NO;
    inwkt->description = "ASCII file containing a WKT "
	"projection description (- for stdin)";

    inproj4 = G_define_option();
    inproj4->key = "proj4";
    inproj4->type = TYPE_STRING;
    inproj4->required = NO;
    inproj4->description = "PROJ.4 projection description (- for stdin)";
#endif

    create = G_define_flag();
    create->key = 'c';
    create->description = "Create new projection files (modifies current "
	"location unless 'location' option specified)";

    location = G_define_option();
    location->key = "location";
    location->type = TYPE_STRING;
    location->required = NO;
    location->description = "Name of new location to create";

    if (G_parser(argc, argv))
	exit(-1);

    /* Always read projection information from current location first */
    G_get_default_window(&old_cellhd);

    if (old_cellhd.proj != PROJECTION_XY) {
        old_projinfo = G_get_projinfo();
	old_projunits = G_get_projunits();
    }

#ifdef HAVE_OGR
    importformats = ((ingeo->answer ? 1 : 0) + (inwkt->answer ? 1 : 0) +
		     (inproj4->answer ? 1 : 0));

    /* -e implies -w */
    if(esristyle->answer && ! printwkt->answer)
	printwkt->answer = 1;

    if (importformats == 0) {
#endif       
        /* Use data from the current location */
        G_get_default_window(&cellhd);
        projinfo = old_projinfo;
        projunits = old_projunits;
#ifdef HAVE_OGR       
    }
    else if (importformats > 1)
	G_fatal_error("Only one of '%s', '%s' or '%s' options may be specified",
		      ingeo->key, inwkt->key, inproj4->key);
    else if (inwkt->answer) {
	FILE *infd;
	char buff[8192];
	char *wktstring;
	int interactive = 1;

	if (strcmp(inwkt->answer, "-") == 0) {
	    infd = stdin;
	    interactive = 0;
	}
	else
	    infd = fopen(inwkt->answer, "r");

	if (infd) {
	    fgets(buff, 8192, infd);
	    fclose(infd);
	    G_asprintf(&wktstring, buff);
	}
	else
	    G_fatal_error("Unable to open file %s for reading", inwkt->answer);

	GPJ_wkt_to_grass(&cellhd, &projinfo, &projunits, wktstring,
			 interactive);
	G_free(wktstring);
    }
    else if (inproj4->answer) {
	FILE *infd;
	char buff[8192];
	char *proj4string;
	int interactive = 1;
	OGRSpatialReferenceH *hSRS;

	if (strcmp(inproj4->answer, "-") == 0) {
	    infd = stdin;
	    fgets(buff, 8192, infd);
	    G_asprintf(&proj4string, "%s +no_defs", buff);
	    interactive = 0;
	}
	else
	    G_asprintf(&proj4string, "%s +no_defs", inproj4->answer);

        /* Set finder function for locating OGR csv co-ordinate system tables */
        SetCSVFilenameHook( GPJ_set_csv_loc );
       
	hSRS = OSRNewSpatialReference(NULL);
	if (OSRImportFromProj4(hSRS, proj4string) < 0)
	    G_fatal_error("Can't parse PROJ.4-style parameter string");

	G_free(proj4string);

	GPJ_osr_to_grass(&cellhd, &projinfo, &projunits, hSRS, interactive);

	OSRDestroySpatialReference(hSRS);
    }
    else {			/* georeferenced file */

	GDALDatasetH gdal_ds;

	/* Try opening file with GDAL first */
	fprintf(stderr, "Trying to open with GDAL...");
	GDALAllRegister();

	if ((gdal_ds = GDALOpen(ingeo->answer, GA_ReadOnly))) {
	    char *wktstring;

	    fprintf(stderr, "...succeeded.\n");
	    wktstring = (char *)GDALGetProjectionRef(gdal_ds);
	    GPJ_wkt_to_grass(&cellhd, &projinfo, &projunits, wktstring, 1);
	}
	else {
	    /* Try opening with OGR */
	    OGRDataSourceH ogr_ds;

	    fprintf(stderr, "Trying to open with OGR...");
	    OGRRegisterAll();

	    if ((ogr_ds = OGROpen(ingeo->answer, FALSE, NULL))
		&& (OGR_DS_GetLayerCount(ogr_ds) > 0)) {
		OGRLayerH ogr_layer;
		OGRSpatialReferenceH *ogr_srs = NULL;

		fprintf(stderr, "...succeeded.\n");
		/* Get the first layer */
		ogr_layer = OGR_DS_GetLayer(ogr_ds, 0);
		ogr_srs = OGR_L_GetSpatialRef(ogr_layer);
		GPJ_osr_to_grass(&cellhd, &projinfo, &projunits, ogr_srs, 1);

		OGR_DS_Destroy(ogr_ds);
		 /* OSRDestroySpatialReference(ogr_srs); */ /* crashes */
	    }
	    else
		G_fatal_error("Could not read georeferenced file %s using "
			      "either GDAL nor OGR", ingeo->answer);
	}

	if (cellhd.proj == PROJECTION_XY)
	    G_warning("Read of file %s was successful, but it did not contain "
		      "projection information. 'XY (unprojected)' will be used",
		      ingeo->answer);
    }
   
    if( importformats == 1 ) {     
        /* If importing projection there will be no region information, so
	 * set some default values */
        cellhd.rows    = 1 ;
        cellhd.rows3   = 1 ;
        cellhd.cols    = 1 ;
        cellhd.cols3   = 1 ;
        cellhd.depths  = 1.;
        cellhd.north   = 1.;
        cellhd.ns_res  = 1.;
        cellhd.ns_res3 = 1.;
        cellhd.south   = 0.;
        cellhd.west    = 0.;
        cellhd.ew_res  = 1.;
        cellhd.ew_res3 = 1.;
        cellhd.east    = 1.;
        cellhd.top     = 1.;
        cellhd.tb_res  = 1.;
        cellhd.bottom  = 0.;
    }
#endif

    if ((cellhd.proj != PROJECTION_XY)
	&& (projinfo == NULL || projunits == NULL))
	G_fatal_error("Projection files missing");

    if ((cellhd.proj == PROJECTION_XY) && (printinfo->answer
#ifdef HAVE_OGR
					   || printwkt->answer
#endif
					   || printproj4->answer ||
					   datuminfo->answer))
	fprintf(stdout, "XY location (unprojected)\n");
    else {
	if (printinfo->answer) {
	    int i;

	    fprintf(stdout,
		    "-PROJ_INFO-------------------------------------------------\n");
	    for (i = 0; i < projinfo->nitems; i++)
		fprintf(stdout, "%-11s: %s\n",
			projinfo->key[i], projinfo->value[i]);

	    fprintf(stdout,
		    "-PROJ_UNITS------------------------------------------------\n");
	    for (i = 0; i < projunits->nitems; i++)
		fprintf(stdout, "%-11s: %s\n",
			projunits->key[i], projunits->value[i]);
	}

	if (datuminfo->answer) {
	    char *datum, *params;
	    struct gpj_datum dstruct;
	    int validdatum = 0;

	    GPJ__get_datum_params(projinfo, &datum, &params);

	    if (datum)
		validdatum = GPJ_get_datum_by_name(datum, &dstruct);

	    if (validdatum > 0)
		fprintf(stdout, "GRASS datum code: %s\nWKT Name: %s\n",
			dstruct.name, dstruct.longname);
	    else if (datum)
		fprintf(stdout, "Invalid datum code: %s\n", datum);
	    else
		fprintf(stdout, "Datum name not present\n");

	    if (params)
		fprintf(stdout,
			"Datum transformation parameters (PROJ.4 format):\n"
			"\t%s\n", params);
	    else if (validdatum > 0) {
		char *defparams;

		GPJ_get_default_datum_params_by_name(dstruct.name, &defparams);
		fprintf(stdout,
			"Datum parameters not present; default for %s is:\n"
			"\t%s\n", dstruct.name, defparams);
		G_free(defparams);
	    }
	    else
		fprintf(stdout, "Datum parameters not present\n");

	    if (validdatum > 0)
		GPJ_free_datum(&dstruct);
	}

	if (printproj4->answer) {
	    struct pj_info pjinfo;
	    char *projstring, *i;

	    pj_get_kv(&pjinfo, projinfo, projunits);
	    projstring = pj_get_def(pjinfo.pj, 0);
	    pj_free(pjinfo.pj);

	    for (i = projstring; *i; i++) {
		/* Don't print the first space */
		if (i == projstring && *i == ' ')
		    continue;

		if (*i == ' ' && !(dontprettify->answer))
		    fputc('\n', stdout);
		else
		    fputc(*i, stdout);
	    }
	    fputc('\n', stdout);
	    G_free(projstring);
	}

#ifdef HAVE_OGR
	if (printwkt->answer) {
	    char *outwkt = GPJ_grass_to_wkt(projinfo, projunits,
					    esristyle->answer,
					    !(dontprettify->answer));
	    if(outwkt != NULL) {
	        fprintf(stdout, "%s\n", outwkt);
	        G_free(outwkt);	       
	    }
	    else
	        G_warning("%s: Unable to convert to WKT", G_program_name() );
	}
#endif
       
        if (create->answer) {
	    
	    if(location->answer) {
	        if( G_make_location( location->answer, &cellhd, 
                                     projinfo, projunits, NULL ) == 0)
		    fprintf(stderr, "Location %s created!\n", location->answer);
	    }
	    else {
	        /* Create flag given but no location specified; overwrite
		 * projection files for current location */

	        if(old_projinfo && old_projunits) {
		    /* Warn as in g.setproj before overwriting current location */
		    fprintf(stderr, "\n\nWARNING!  A projection file already exists for this location\n");
		    fprintf(stderr, "\nThis file contains all the parameters for the\nlocation's projection: %s\n", G_find_key_value("proj", old_projinfo));
		    fprintf(stderr, "\n    Overriding this information implies that the old projection parameters\n");
		    fprintf(stderr, "    were incorrect.  If you change the parameters, all existing data will be\n");
		    fprintf(stderr, "    interpreted differently by the projection software.\n%c%c%c", 7, 7, 7);
		    fprintf(stderr, "    GRASS will not re-project your data automatically\n\n");

		    if (G_yes("Would you still like to overwrite the current projection information ", 0)) {
		       
		        int out_stat;
		        char path[4096];
		       
                        /* Create the default, and current window files */
                        G__put_window( &cellhd, "", "DEFAULT_WIND" );
                        G__put_window( &cellhd, "", "WIND" );

                        /* Write out the PROJ_INFO, and PROJ_UNITS if available. */
                        if( projinfo != NULL ) {
                            G__file_name( path, "", "PROJ_INFO", "PERMANENT" );
                            G_write_key_value_file( path, projinfo, &out_stat );
                            if( out_stat != 0 )
                                G_fatal_error("Error writing PROJ_INFO");
                        }

                        if( projunits != NULL ) {
                            G__file_name( path, "", "PROJ_UNITS", "PERMANENT" );
                            G_write_key_value_file( path, projunits, &out_stat );
                            if( out_stat != 0 )
                                G_fatal_error("Error writing PROJ_UNITS");
			}
		        fprintf(stderr, "Projection information updated!\n");
		    }
		    else
		        fprintf(stderr, "The projection information will not be updated.\n");
	       	       
		}
	    }
	}
    }

    if (projinfo != NULL)
	G_free_key_value(projinfo);
    if (projunits != NULL)
	G_free_key_value(projunits);
    return 0;

}
