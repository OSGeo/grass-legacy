/*
 * $Id$
 * @(#)main.c	1.1   02/28/90
 * @(#)main.c	1.0   08/1/87
 * purpose:  Program for converting SCS MIADS format cell data to
*            r.in.ascii, rev 4.0 (Mimport.ll, rev3.0) input format.
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             r.miadin  input=Miads data file to read 
*                       output=raster data file to create
*                       strip=Miads file strip number of reference cell
*                       line=Miads file line number of reference cell
*                       cell=Miads file cell number of reference cell
*                       north=UTM Northing at the cell reference
*                       east=UTM Easting at the cell reference
*                       size=Cell size(length one side) in meters
*
*/

#include <unistd.h>
#include "gis.h"
#include "miad.h"

int 
main (int argc, char *argv[])
{
	struct MIAD_INFO Mhd ;
        struct Option *inopt, *outopt, *strpopt, *lineopt, *cellopt;
	struct Option *Nopt, *Eopt, *sizeopt;
	int  i ;
	int fdI, fdO;
	struct GModule *module;

        G_gisinit (argv[0]);
        
        /* Set description */
        module              = G_define_module();
        module->description = ""\
        "Imports SCS MIADS format raster data into GRASS raster map layer";

        /* set up the options for the command line parser */
        inopt = G_define_option();
        inopt->key             = "input";
        inopt->type            = TYPE_STRING;
        inopt->required        = YES;
        inopt->description     = "Miads input file name ";

        outopt = G_define_option();
        outopt->key             = "output";
        outopt->type            =  TYPE_STRING;
        outopt->required        =  YES;
        outopt->description     = "GRASS raster data output file name ";

        strpopt = G_define_option();
        strpopt->key              = "strip";
        strpopt->type             =  TYPE_INTEGER;
        strpopt->required         =  YES;
        strpopt->description      = "Miads strip number of reference cell";

        lineopt = G_define_option();
        lineopt->key              = "line";
        lineopt->type             =  TYPE_INTEGER;
        lineopt->required         =  YES;
        lineopt->description      = "Miads line number of reference cell";

        cellopt = G_define_option();
        cellopt->key              = "cell";
        cellopt->type             =  TYPE_INTEGER;
        cellopt->required         =  YES;
        cellopt->description      = "Miads cell number of reference cell";

        Nopt = G_define_option();
        Nopt->key              = "Northing";
        Nopt->type             =  TYPE_DOUBLE;
        Nopt->required         =  YES;
        Nopt->description      = "UTM Easting at the cell reference";

        Eopt = G_define_option();
        Eopt->key              = "Easting";
        Eopt->type             =  TYPE_DOUBLE;
        Eopt->required         =  YES;
        Eopt->description      = "UTM Northing at the cell reference";

        sizeopt = G_define_option();
        sizeopt->key              = "size";
        sizeopt->type             =  TYPE_INTEGER;
        sizeopt->required         =  YES;
        sizeopt->description      = "Cell size(length one side) in meters";


           /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
            exit (-1);

          /* Check for files existance  */
	if ((fdI = access(inopt->answer,4)) != 0)
		{
		fprintf(stderr,"\n\n\tThe input file %s can NOT be accessed !\n",inopt->answer);
		exit(-1);
		}
	/*
	     Check for files non-existance */
       	if ((fdO = access(outopt->answer,0)) != -1)
		{
		fprintf(stderr,"\n\n\tThe output file %s ALREADY exists!\n",outopt->answer);
		exit(-1);
		}
    
        sscanf(strpopt->answer,"%d",&Mhd.ORIGIN_STRIP) ;
        sscanf(lineopt->answer,"%d",&Mhd.ORIGIN_LINE) ;
        sscanf(cellopt->answer,"%d",&Mhd.ORIGIN_CELL) ;
        sscanf(Nopt->answer,"%lf",&Mhd.UTM_NORTH) ;
        sscanf(Eopt->answer,"%lf",&Mhd.UTM_EAST) ;
        sscanf(sizeopt->answer,"%d",&Mhd.CELL_SIZE) ;
        sprintf(Mhd.MIADS_MAP_NAME,"%s",inopt->answer) ;
        sprintf(Mhd.OUTPUT_FILE   ,"%s",outopt->answer) ;

  	if ((i = proces(&Mhd)) != 0)
		{
		fprintf (stdout," Error in process routine\n");
		} 


	exit(0);

}
