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
*            date parm, cleanup: Markus Neteler (1/2002)
* PURPOSE:  module to import DBF-tables as sites list
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "shapefil.h"
#include "local_proto.h"
#include <ctype.h> 

static char *G_extract_base_name(char *, const char * );


int main( int   argc, char *argv[])
{
    char *infile;
    char name[512], outfile[512];

    struct {
	struct Option *input, *output, *date;
	/*struct Option *date, *order;*/
    } parm;
    struct GModule *module;
    struct Flag *listflag, *third;
    struct TimeStamp ts;
    DBFHandle   hDBF;
    char *buf=NULL;
     
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

    parm.date = G_define_option();
    parm.date->key = "date";
    parm.date->key_desc = "timestamp";
    parm.date->required = NO;
    parm.date->type = TYPE_STRING;
    parm.date->description = "datetime or none (default: none)";

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

    /* treat third column as third dimension */
    third = G_define_flag();
    third->key      = 'z';
    third->description = "treat third column as third dimension (z)";


    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    if(!parm.output->answer)
    {
        /* to avoid crash with absol. paths */
        strcpy(name, parm.input->answer);
        G_extract_base_name(outfile, name);
    }
    else
	strcpy(outfile, parm.output->answer);

    if (parm.date->answer)
       G_scan_timestamp (&ts, parm.date->answer);
    else
       G_init_timestamp(&ts);
    
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
	  int   field_width; 
	  char  *fld=NULL; 
          DBFFieldType ftype;

          ftype=DBFGetFieldInfo( hDBF, i, field_name, &field_width, NULL );

	  switch (ftype) {
		case 0:
			fld="text";
		break;
		case 1:
			if (field_width<=7) fld="int4";
				else fld="int8";
		break;
		case 2:
			fld="float4";
		break;
		case 3:
            		G_fatal_error ("Invalid field type - bailing out");
		break;
	  }

	  DBFGetFieldInfo( hDBF, i, field_name, NULL, NULL );
	  fprintf (stdout, "%i: %s [%s:%i]\n", (i+1), field_name, fld , field_width);
        }
        
      DBFClose( hDBF );

    }
    else
       DumpFromDBF(infile, outfile, ts, third->answer);

    exit(0);
}


/* from v.in.shape (David D Gray, should go into gis/strings.h */

static char *G_extract_base_name(char *base_name, const char *in_name ) {
  
  /* Extract the basename of a fullname of the input file. ie. the answer to
     the option `in'
  */

  char *name;
  char *p;

  name = (char *)malloc(strlen(in_name)+1);
  strcpy( name, in_name );

  for( p = name+strlen(name)-1;
       p != name-1 && (isalnum(*p) || *p == '_' || *p == '.' );
       p-- ) {}
  strcpy( base_name, p+1);
  free(name);
    
  p = strrchr( base_name, '.');
  if (p != NULL)
    *p = '\0';
  base_name = p;
  return base_name;
}
