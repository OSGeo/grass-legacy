/*v.to.pg 
Alex Shevlakov <sixote@yahoo.com>, 03/2002.
Exports polygons and lines to new Postgres table as types "polygon" and "path"*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "dbvect.h"
#include "glocale.h"

#define MAIN

char *map_string;
char *key_string;
char *table_string;
char *vtype_string;
int total_vects=0;
int total_vertices=0;
int total_import=0;
int verbose=0;
int to_postgis=0;

int main(argc, argv)
int argc ;
char **argv ;
{

    char *dbname;

    int i;

    int selPassed;	/* User specified select inputfile */
    


	selPassed = 0;


	G_init_locale();

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;


	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                  "Please run g.select.pg to identify a current database.\n");
	    exit(-1);
           }


	/* Check for -s flag indicating selectfile input */
	for (i=0; i<argc; i++)
 		if(strcmp(argv[i],"-s")==0) 
			selPassed = 1;
			

	if (selPassed)        	/* user provides SQL command file 	*/
			getSelectOpts(argc,argv) ;

	  else			/*  Pgm builds SQL command file 	*/
		 	getAllOpts(argc, argv) ;


	exit(0);
}

