/* MODIFIED by Katarina Johnsson for INGRES version 5.0
 * Mon Apr 11 1993
 */

/* MODFIED by Noel Krahn for INGRES version 6 
 * DATE: Fri Oct 30 15:10:53 PST 1992 
 */


/*
 *   g.table.db
 *
 *
 *   Generate list of database tables in 
 *   the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/19/91
 */

#include <stdio.h>
#include "gis.h"
#define MAIN
#define TABLEN 20

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char sqlFile[100];



	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	sprintf(sqlFile,"/tmp/%d.sql",getpid());

        /* Check command line for help  */
	if ((argc == 2) && (strcmp(argv[1],"help") == 0) ) {
		fprintf(stderr, "\n\nUsage: %s (no additional command line arguements).\n", argv[0]);
		exit(0);
	}

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.db to identify a current database.\n");
	    exit(-1);
           }

	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

  /* Build SQL query to get table names from INGRES version 5.0 */
  fprintf(fp, "SELECT DISTINCT tables=relid\n");
  fprintf(fp, "FROM relation\n");
  fprintf(fp, "WHERE mod(relstat,2) <= 0\n");
  fprintf(fp, "\\g\n");

        fclose(fp);


/*************** INGRES driver code begins *************************/
	ingresTable(sqlFile);

	unlink(sqlFile);

	exit(0);
}

