/* updated to PostgreSQL 6.3 by Markus Neteler 8/98 */

/*
 *   g.table.inf
 *
 *
 *   Generate list of database tables in 
 *   the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/19/91
 */
/*
#----------------------A.Sh -12.99
**	Name : g.table.pg
**
**      Author : J.Soimasuo
**	Description : g.table.inf for Postgres. env variable PG_DBASE.
**	Date : 14th March 1994
*/


#include <stdio.h>
#include "gis.h"
#define MAIN
#define TABLEN 20
/*#define SELECT "SELECT ( pg.relname ) from pg in pg_class where pg.relname !~ \"pg_\" sort by relname";*/
#define SELECT "select relname from pg_class where relname !~ \"pg_\" order by relname";   

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *select;  
    char sqlFile[100];


	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

        /* Check command line for help  */
	if ((argc == 2) && (strcmp(argv[1],"help") == 0) ) {
		fprintf(stderr, "\n\nUsage: %s (without arguments!).\n", argv[0]);
		exit(0);
	}

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.pg to identify a current database.\n");
	    exit(-1);
           }

/*************** INFX driver code begins ***************/
	infxTables();

	exit(0);
}

