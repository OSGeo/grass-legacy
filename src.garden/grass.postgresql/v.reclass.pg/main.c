/*
 *   v.reclass.inf
 *
 *
 *   Generate a reclass vector using the results of
 *   one or more database queries.
 *
 *   jaf 4/5/93
 */
/*
**	Name : v.reclass.pg
**
**	Description :v.reclass.inf for postgres. 
**	
**
**	Author: Janne Soimasuo
**	Date: 14th March 1994 
*/
 
/*
*	modified to libpq alex shevlakov - jan'00
*/

#include <stdio.h>
#include "gis.h"
#define MAIN


main(argc, argv)
int argc ;
char **argv ;
{
    char *dbname;

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
