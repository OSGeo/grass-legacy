/*
** Code Compiled by Jo Wood [JWO] 26th October 1991
** Midlands Regional Research Laboratory (ASSIST)
**
**
*/

/* %W% %G% */
#include "gis.h"

main(argc,argv) 
    int argc;
    char *argv[];
{

    /****** INITIALISE ******/


    struct Option 	*x;		/* Structures required for the G_parser()	*/
   					/* call. These can be filled with the		*/
    struct Option	*y;		/* various defaults, mandatory paramters	*/
					/* etc. for the GRASS user interface.		*/

    G_gisinit (argv[0]);	/*	This GRASS library function MUST
					be called first to check for valid
					database and mapset. As usual argv[0]
					is the program name. This can be
					recalled using G_program_name(). */

    /****** SET PARSER OPTIONS ******/

    x  = G_define_option(); 	/*	Request pointer to memory for each option.	*/
    y  = G_define_option();	/* 	- Names of files holding x and y coordinates	*/

    x->key		= "x";
    x->description	= "Name of the raster map layer to hold x coordinate values";
    x->type		= TYPE_STRING;
    x->required	= YES;

    y->key		= "y";
    y->description	= "Name of the raster map layer to hold y coordinate values";
    y->type		= TYPE_STRING;
    y->required	= YES;


    if (G_parser(argc,argv))
	exit(-1);		/*	Returns a 0 if sucessful		*/


    /****** CHECK THE CELL FILES (X & Y) DO NOT ALREADY EXIST******/

    if (G_legal_filename(x->answer)==NULL)
    {
	char err[256];
	sprintf(err,"Illegal file name. Please try another.");
	G_fatal_error(err);
    }
    else
    {
	if (G_find_cell(x->answer,"") !=NULL)
	{
	    char err[256];
	    sprintf(err,"Raster map [%s] already exists.\nPlease try another.",x->answer);
	    G_fatal_error(err);
	}

    }

    if (G_legal_filename(y->answer)==NULL)
    {
	char err[256];
	sprintf(err,"Illegal file name. Please try another.");
	G_fatal_error(err);
    }
    else
    {
	if (G_find_cell(y->answer,"") !=NULL)
	{
	    char err[256];
	    sprintf(err,"Raster map [%s] already exists.\nPlease try another.",y->answer);
	    G_fatal_error(err);
	}

    }


    /****** CREATE THE X and Y CELL FILES  ******/

    createxy(x->answer,y->answer);


}
