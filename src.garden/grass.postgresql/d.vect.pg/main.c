/*
 *   d.vect.inf
 *
 *
 *   Display vector(s) associated the results of a
 *   database query.
 *
 *   jaf 2/19/92
 */

/*   modifcations 11/98  after '94 update
       d.site.pg
       update to libpq
       recode for Postgres v6.4
       add error messages in run routines
       
     Carl Anderson
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "dbvect.h"
#include "glocale.h"

#define MAIN


int main(argc, argv)
int argc ;
char **argv ;
{

    char *dbname;

    int i;

    int selPassed;	/* User specified select inputfile */

	selPassed = 0;


	G_init_locale(PACKAGE);

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

