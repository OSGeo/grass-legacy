/*
* $Id$
*
****************************************************************************
*
* MODULE:   s.in.dbf - module to import DBF-tables as sites file
* AUTHOR(S): Markus Neteler  neteler@geog.uni-hannover.de
*            DBF-routines: Frank Warmerdam
*            Import concept: Alex Shevlakov (pg.in.dbf)
*            -l flag: David Gray (v.in.shape)
* PURPOSE:  module to import DBF-tables as sites list
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>
#include "gis.h"
#include "shapefil.h"

int main( int   argc, char *argv[])
{
    char *infile, *outfile;

    struct {
	struct Option *input, *output;
	/*struct Option *order;*/
    } parm;
    struct GModule *module;
    struct Flag *listflag;
    DBFHandle   hDBF;
    char *buf;
     
    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

    /* define the different options */

    module = G_define_module();
    module->description = 
      "Import a dBase table of site locations "
      "into a GRASS site list file.";


    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of .dbf file to be imported";

    parm.output = G_define_option();
    parm.output->key = "sites";
    parm.output->type = TYPE_STRING;
    parm.output->required = NO;
    parm.output->description = "sites file to be created";
    parm.output->gisprompt = "any,site_lists,sites";

/* not yet implemented. See dump.c DBFDumpASCII as well.
    parm.order = G_define_option() ;
    parm.order->key        = "order";
    parm.order->type       = TYPE_INTEGER;
    parm.order->required   = NO;
    parm.order->multiple   = YES;
    parm.order->description= "fields order comma separated (e.g. 1,3,2,4)";
*/

    /* Set flag for listing fields of database */

    listflag = G_define_flag();
    listflag->key     = 'l';
    listflag->description = "List fields of DBF file";



    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    if(!parm.output->answer)
    	outfile = parm.input->answer;
    else
	outfile = parm.output->answer;

    /* Examine the `-l' flag: Borrowed from David Gray's v.in.shape */
    if(listflag->answer) {
      int	i;
        
      hDBF = DBFOpen( infile, "r" );
      if( hDBF == NULL )
        {
	  sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
	  G_fatal_error (buf);
        }

      fprintf (stdout, "Attribute fields available in %s:\n", infile );
      for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
	  char	field_name[15];

	  DBFGetFieldInfo( hDBF, i, field_name, NULL, NULL );
	  fprintf (stdout, "%i: %s\n", (i+1), field_name );
        }
        
      DBFClose( hDBF );

    }
    else
        DumpFromDBF(infile, outfile);
    
    exit(0);
}

