/* buildSQL - build the SQL query, execute it and display the result
 * 
 * INGRES modifications - '\g' added to SQL query.
 * 			  No modifications in SQL syntax needed.
			  Syntax for execution of SQL command modified.
			  Modifications apply to INGRES ver5.0 and ver6.
 * 
 * KJ 930415
 */

#include "gis.h"


buildSQL(ktab,keycat, joinargs, curcat)
	struct Option *ktab, *keycat; 
	char *joinargs[];
	int curcat;
{
	FILE *fp;
	char buf[1024];
	char sqlFile[100] ;


	sprintf(sqlFile,"/tmp/%d.sql", getpid() );

        /* Open file to write SQL commands */
        if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (sql).\n");
            exit(-1);
           }

	printf("Building SQL query ... \n");

	/* \g added to SQL query for INGRES. No syntax changes, KJ 930415 */
	if (joinargs)  {	/* two table qry with joinkey */
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",joinargs[0]);
		fprintf(fp,"WHERE %s in\n",joinargs[1]);
		fprintf(fp,"(select %s from %s \n",
			joinargs[2],ktab->answer);
		fprintf(fp,"WHERE ( %s = %d ) )\n",
			keycat->answer, curcat);
		fprintf(fp,"\\g\n");

	}
	else  {
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( %s = %d )\n",
			keycat->answer, curcat);
		fprintf(fp,"\\g\n");
	}

        
	fclose (fp);
 
  	printf("Querying database ...\n");
        sprintf(buf,"sql -s %s < %s 2>/dev/null", G_getenv("DATABASE"), sqlFile);

        /* Use the following to see DB output number of rows returned
        sprintf(buf,"sql -s %s <  %s 2>&1", G_getenv("DATABASE"), sqlFile);
        */

	if(isatty(1)) strcat (buf," | more");
	system(buf);

	unlink(sqlFile) ;

	return 0 ;
}


