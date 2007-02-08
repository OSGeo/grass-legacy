/*  
 ****************************************************************************
 *
 * MODULE:       g.proj 
 * AUTHOR(S):    Paul Kelly - paul-grass@stjohnspoint.co.uk
 * PURPOSE:      Provides a means of reporting the contents of GRASS
 *               projection information files and creating
 *               new projection information files.
 * COPYRIGHT:    (C) 2003-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>

#define G_PROJ_MAIN
#include "local_proto.h"

int main(int argc, char *argv[])
{
    struct Flag *printinfo,    	/* Print contents of PROJ_INFO & PROJ_UNITS */
        *printproj4,		/* Print projection in PROJ.4 format        */
        *datuminfo,		/* Check if datum information is present    */
        *create,		/* Create new projection files              */
        *printwkt,		/* Print projection in WKT format           */
        *esristyle,		/* Use ESRI-style WKT format                */
        *dontprettify;		/* Print 'flat' output (no linebreaks)      */

    struct Option *location,	/* Name of new location to create           */
	  *inepsg,		/* EPSG code to create new location with */
	  *dtrans,		/* index to datum transform option */
          *inwkt,	        /* Input file with projection in WKT format */
          *inproj4,		/* Projection in PROJ.4 format              */
          *ingeo;		/* Input geo-referenced file readable by 
				 * GDAL or OGR                              */
    struct GModule *module;
   
    int importformats;
    char buffer[64];

    G_set_program_name(argv[0]);
    G_no_gisinit();

    module = G_define_module();
    module->keywords = _("general");
    module->description =
	_("Converts co-ordinate system descriptions (i.e. projection "
	  "information) between various formats (including GRASS format).");

    printinfo = G_define_flag();
    printinfo->key = 'p';
    printinfo->guisection = "Printed_Output";
    printinfo->description =
      _("Print projection information (in conventional GRASS format)");

    datuminfo = G_define_flag();
    datuminfo->key = 'd';
    datuminfo->guisection = "Printed_Output";
    datuminfo->description =
      _("Verify datum information and print transformation parameters");

    printproj4 = G_define_flag();
    printproj4->key = 'j';
    printproj4->guisection = "Printed_Output";
    printproj4->description =
      _("Print projection information in PROJ.4 format");

    printwkt = G_define_flag();
    printwkt->key = 'w';
    printwkt->guisection = "Printed_Output";
    printwkt->description =
      _("Print projection information in WKT format");

    esristyle = G_define_flag();
    esristyle->key = 'e';
    esristyle->guisection = "Printed_Output";
    esristyle->description =
      _("Use ESRI-style format (applies to WKT output only)");

    dontprettify = G_define_flag();
    dontprettify->key = 'f';
    dontprettify->guisection = "Printed_Output";
    dontprettify->description =
      _("Print 'flat' output with no linebreaks (applies to WKT and "
        "PROJ.4 output)");

    ingeo = G_define_option();
    ingeo->key = "georef";
    ingeo->type = TYPE_STRING;
    ingeo->key_desc = "file";
    ingeo->required = NO;
    ingeo->guisection = "Input";
    ingeo->description = _("Georeferenced data file to read projection "
	"information from");

    inwkt = G_define_option();
    inwkt->key = "wkt";
    inwkt->type = TYPE_STRING;
    inwkt->key_desc = "file";
    inwkt->required = NO;
    inwkt->guisection = "Input";
    inwkt->description = _("ASCII file containing a single line WKT "
	"projection description (- for stdin)");

    inproj4 = G_define_option();
    inproj4->key = "proj4";
    inproj4->type = TYPE_STRING;
    inproj4->key_desc = "params";
    inproj4->required = NO;
    inproj4->guisection = "Input";
    inproj4->description = _("PROJ.4 projection description (- for stdin)");

    create = G_define_flag();
    create->key = 'c';
    create->guisection = "Create/Edit_Locations";
    create->description = _("Create new projection files (modifies current "
	"location unless 'location' option specified)");

    inepsg = G_define_option();
    inepsg->key = "epsg";
    inepsg->type = TYPE_INTEGER;
    inepsg->required = NO;
    inepsg->options  = "1-100000";
    inepsg->guisection = "Create/Edit_Locations";
    inepsg->description = _("EPSG code of projection to be created");

    dtrans = G_define_option();
    dtrans->key = "datumtrans";
    dtrans->type = TYPE_INTEGER;
    dtrans->required = NO;
    dtrans->options  = "0-100";
    dtrans->answer   = "1";
    dtrans->guisection = "Create/Edit_Locations";
    dtrans->description = _("Index number of datum transform parameter, "
	"or \"0\" for a list");

    location = G_define_option();
    location->key = "location";
    location->type = TYPE_STRING;
    location->key_desc = "name";
    location->required = NO;
    location->guisection = "Create/Edit_Locations";
    location->description = _("Name of new location to create");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    /* Initialisation & Validation */

    /* -e implies -w */
    if(esristyle->answer && ! printwkt->answer)
	printwkt->answer = 1;
   
    projinfo = NULL;
    projunits = NULL;
   
    importformats = ((ingeo->answer? 1 : 0) + (inwkt->answer? 1 : 0) +
		     (inproj4->answer? 1 : 0) + (inepsg->answer? 1 : 0) );
    if (importformats > 1)
	G_fatal_error(_("Only one of '%s', '%s' or '%s' options may be specified"),
		      ingeo->key, inwkt->key, inproj4->key);

    if (strcmp(dtrans->answer, "0") == 0) {
	/* list available datum transform options */
    /* TODO: check if EPSG code exists */
	if(!inepsg->answer)
	    G_fatal_error(_("EPSG code <%s> does not exist"), inepsg->answer);
    /* Does this need to work with just EPSG codes, or with any input method? */
	G_message("Datum transform parm list still under construction.\n"
		  "Please try again later.");
	exit (EXIT_SUCCESS);
    }

    /* Input */
    /* We can only have one input source, hence if..else construct */
   
    if (importformats == 0)
        /* Input is projection of current location */
        input_currloc();
    else if (inwkt->answer)
        /* Input in WKT format */
        input_wkt(inwkt->answer);
    else if (inproj4->answer)
        /* Input in PROJ.4 format */
        input_proj4(inproj4->answer);
    else if (inepsg->answer) {
	/* Input from EPSG code */
	sprintf(buffer, "+init=epsg:%s", inepsg->answer);
	input_proj4(buffer);
    }
    else 
        /* Input from georeferenced file */
        input_georef(ingeo->answer);


    /* Consistency Check */
   
    if ((cellhd.proj != PROJECTION_XY)
	&& (projinfo == NULL || projunits == NULL))
	G_fatal_error(_("Projection files missing"));

   
    /* Output */
    /* We can output the same information in multiple formats if
     * necessary, hence multiple if statements */
   
    if (printinfo->answer)
	print_projinfo();

    if (datuminfo->answer)
	print_datuminfo();

    if (printproj4->answer)
	print_proj4(dontprettify->answer);

    if (printwkt->answer) 
	print_wkt(esristyle->answer, dontprettify->answer);
       
    if (create->answer)
	create_location(location->answer);       


    /* Tidy Up */
   
    if (projinfo != NULL)
	G_free_key_value(projinfo);
    if (projunits != NULL)
	G_free_key_value(projunits);
   
    exit(EXIT_SUCCESS);

}
