/*
 *   d.site.inf
 *
 *
 *   Display site(s) locations associated the results of a
 *   database query.
 *
 *   jaf 2/3/92
 */

/*  d.site.pg
      after mods in '94
      modifications 11/98
        to support v6.4 of Postgress
        use libpq instead of psql
        use pghost,pgdbase
        allow cats support through interface if three columns in query
*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "dbsite.h"
#include "glocale.h"

#define MAIN

int main(argc, argv)
int argc ;
char **argv ;
{

    char *dbname;

    int i;
    int selPassed;      /* User specified select inputfile [-s] */

#ifdef HAVE_LIBINTL_H
  setlocale (LC_MESSAGES, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
#endif


	selPassed = 0;


	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                  _("Please run g.select.pg to identify a current database.\n"));
	    exit(-1);
           }


        /* Check for -s flag indicating selectfile input */
        for (i=0; i<argc; i++)
                if(strcmp(argv[i],"-s")==0)
                        selPassed = 1;


        if (selPassed) {          /* user provides SQL command file       */
                	i=getSelectOpts(argc,argv);
			if ( i < 0 ) exit(-1);
		}
          else  {                /*  Pgm builds SQL command file         */
                	i = getAllOpts(argc, argv);
			if ( i < 0 ) exit(-1);
		}

exit(0);
}
