/*
** Code Compiled by Jo Wood [JWO] 23rd October 1991
** Midlands Regional Research Laboratory (ASSIST)
**
**
*/

/* %W% %G% */
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{

    /****** INITIALISE ******/

	struct GModule *module;
    struct Option 	*out;		/* Structures required for the G_parser()	*/
   					/* call. These can be filled with the		*/
    struct Option	*min;		/* various defaults, mandatory paramters	*/
					/* etc. for the GRASS user interface.		*/
    struct Option	*max;

    G_gisinit (argv[0]);	/*	This GRASS library function MUST
					be called first to check for valid
					database and mapset. As usual argv[0]
					is the program name. This can be
					recalled using G_program_name(). */

	module = G_define_module();
    module->description =
		"GRASS module to produce a raster map layer of uniform "
		"random deviates whose range can be expressed by the user. "
		"It uses the random number generator described in Press, "
		"Flannery, Teukolsky and Vetterling (1988) - Numerical "
		"Recipes in C.";

    /****** SET PARSER OPTIONS ******/

    out   = G_define_option(); 	/*	Request pointer to memory for each option.	*/
    min   = G_define_option();	/* 	Minimum random value				*/
    max   = G_define_option();	/* 	Maximum random value				*/

    out->key		= "out";
    out->description	= "Name of the random surface to be produced";
    out->type		= TYPE_STRING;
    out->required	= YES;

    min->key		= "min";
    min->description	= "Minimum random value";
    min->type		= TYPE_INTEGER;
    min->answer		= "0";

    max->key		= "max";
    max->description	= "Maximum random value";
    max->type		= TYPE_INTEGER;
    max->answer		= "100";

    if (G_parser(argc,argv))
	exit(-1);		/*	Returns a 0 if sucessful		*/



    /****** CHECK THE CELL FILE (OUT) DOES NOT ALREADY EXIST******/

    if (G_legal_filename(out->answer)=='\0')
    {
	char err[256];
	sprintf(err,"Illegal file name. Please try another.");
	G_fatal_error(err);
    }
    else
    {
	if (G_find_cell(out->answer,"") !=NULL)
	{
	    G_fatal_error("Raster map [%s] already exists.\nPlease try another.",out->answer);
	}

    }

    /****** CREATE THE RANDOM CELL FILE  ******/

    randsurf(out->answer,atoi(min->answer),atoi(max->answer));

    return 0;
}
