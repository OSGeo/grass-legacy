/* MODFIED by Noel Krahn
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

  /* send isql display database sequence */
  fprintf(fp, "SELECT DISTINCT table_name\n");
  fprintf(fp, "FROM iicolumns\n");
  fprintf(fp, "WHERE table_name NOT LIKE 'ii%%'\n");
  fprintf(fp, "\\g\n");

        fclose(fp);


/*************** INFX driver code begins ***************/
	ingresTable(sqlFile);

	unlink(sqlFile);

	exit(0);
}

