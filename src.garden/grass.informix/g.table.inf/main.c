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

#include <stdio.h>
#include "gis.h"
#define MAIN
#define TABLEN 20
#define SELECT " SELECT tabname FROM systables WHERE tabid  >= 100 ORDER BY tabname";

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

	sprintf(sqlFile,"/tmp/%d.sql",getpid());

        /* Check command line for help  */
	if ((argc == 2) && (strcmp(argv[1],"help") == 0) ) {
		fprintf(stderr, "\n\nUsage: %s (no additional command line arguements).\n", argv[0]);
		exit(0);
	}

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.inf to identify a current database.\n");
	    exit(-1);
           }

	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

 	select = SELECT; 
	fprintf(fp,"%s",select);
        fclose(fp);


/*************** INFX driver code begins ***************/
	infxTables(sqlFile);

	unlink(sqlFile);

	exit(0);
}

