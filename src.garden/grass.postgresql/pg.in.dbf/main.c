#include <stdio.h>
#include "gis.h"
#include "shapefil.h"

/******************************************************************/
 /*02/2000 dbf dump to Postgres
 *	Alex Shevlakov sixote@yahoo.com
 ******************************************************************/


int main( int   argc, char *argv[])
{
 

    int no_rattle;
    char *infile;


    struct {
	struct Option *input, *dumpmode;
    } parm;

    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of .dbf file to be imported";

    
    parm.dumpmode = G_define_option() ;
    parm.dumpmode->key        = "dumpmode";
    parm.dumpmode->type       = TYPE_STRING;
    parm.dumpmode->required   = NO;
    parm.dumpmode->description= "Admin/normal user dump mode (Default = Postgres super-user)";
    

    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    no_rattle = (int) parm.dumpmode->answer;
    

    	PgDumpFromDBF(infile, no_rattle);


    
    exit(0);
}

