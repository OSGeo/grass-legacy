/*
 *   d.vect.inf
 *
 *
 *   Display vector(s) associated the results of a
 *   database query.
 *
 *   jaf 2/19/92
 */

#include "gis.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *colname;  
    char *mapset ;
    char buf[1024];

    int i;
    int retval;
    int selPassed;	/* User specified select inputfile */

	setbuf (stdout, NULL);

	selPassed = 0;
	retval = 0;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;


	/* Check DATABASE env variable */
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                  "Please run g.select.inf to identify a current database.\n");
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

