/*
 *   d.what.v.inf
 *
 *
 *   Generate a list of database attributes
 *   associated with vector feature selected
 *   using the GRASS mouse.
 *
 *   jaf 1/11/91, rev 2/21/92
*/
/*
**	Name : d.what.v.pg
**
**	Description : d.what.v.inf for Postgres
**	Input :
**	Output :
**	Author: Janne Soimasuo
**	Date: 11th March 1994
*/

/*---------------A.Sh. dec.99
			libpq interface 
			clicked lines/areas coloring
			more control of database output
*/

#include "what.h"
#include "Vect.h"
#include <stdlib.h>
#include <string.h>
#include "glocale.h"

int fd;
int dbCat;
int h_num;
char *name;
char *mapset;

int main(argc, argv)
int argc ;
char **argv ;
{
    char *dbname;
    int i;
    int selPassed;      /* User specified select inputfile */

        selPassed = 0;


	G_init_locale();

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


        if (selPassed)          /* user provides SQL command file       */
                getSelectOpts(argc,argv);
          else                  /*  Pgm builds SQL command file         */
                 getAllOpts(argc, argv);

	exit(0) ;
}
