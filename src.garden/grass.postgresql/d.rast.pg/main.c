/*
 *   d.rast.inf
 *
 *
 *   Generate a reclass image using the results of a
 *   database query.
 *
 *   jaf 1/7/92
	
 * d.rast.pg 
 * modified version for postgres by J.Soimasuo 2nd March 1994
 */
 
 /*
 *	modified to libpq by alex shevlakov -dec'99
 */

#include <stdio.h>
#include "gis.h"
#include "dbrast.h"
#define MAIN


main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *colname;  
    char buf[1024];

    int i, stat;
    int selPassed;      /* User specified select inputfile */


        selPassed = 0;
        stat = 0;


	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                  "Please run g.select.pg to identify a current database.\n");
	    exit(-1);
           }


        /* Check for -s flag indicating selectfile input */
        for (i=1; i<argc; i++)
		if(argv[i][0]=='-' && index(argv[i],'s') )
                        selPassed = 1;


        if (selPassed)          /* user provides SQL command file       */
                stat = getSelectOpts(argc,argv);
          else                  /*  Pgm builds SQL command file         */
                stat = getAllOpts(argc, argv);


	exit(0);
}
