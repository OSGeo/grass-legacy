/* runSQL - executes user supplied SQL file.
 *
 * INGRES modifications - '\g' added to the user supplied SQL query
 *			  Syntax for execution of SQL command modified.
 * 
 *			  Modifications apply to both INGRES ver5.0 and ver.6
 *
 * Improvements - A 'where' clause is added to the user supplied query,
 * 		  to restrict the SQL 'select' to the current category, as 
 *		  indicated by the cursor (curcat).
 * 
 * Katarina Johnsson 930413
 */

#include "gis.h"


runSQL(sqlin, keycat, curcat)
	char *sqlin, *keycat;
	int curcat;
{
	FILE *fpin, *fpout;
	char buf[1024];
	char ch;
	char sqlFile[100] ;

	sprintf(sqlFile, "/tmp/%d.sql", getpid() );

        /* Open file to read SQL commands */
        if((fpin = fopen(sqlin,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql).\n");
            exit(-1);
           }

        /* Open file to write SQL commands  with cahracter substitution */
        if((fpout = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (sql output).\n");
            exit(-1);
           }

/* Remove to test my way
         Read SQL input and do character substitution 
        while (!feof(fpin) ) {
		ch = getc(fpin);
		if(ch == '?') fprintf(fpout,"%d",curcat);
		else fprintf(fpout,"%c",ch);
	}
*/ 

	/* Add key column to query, test added by kj Nov, 4 1992 */
	ch = getc(fpin);
	while (!feof(fpin) ) {
		fprintf(fpout, "%c",ch);
		ch = getc(fpin);
	}
	fprintf(fpout,"where (%s=%d)\n", keycat, curcat);

	/* Added to run with INGRES. KJ 930413 */
	fprintf(fpout,"\\g\n");
        
	fclose(fpout);
	fclose (fpin);

	/* Syntax modified for INGRES. KJ 930413 */
        sprintf(buf,"sql -s %s <  %s 2>/dev/null", G_getenv("DATABASE"), sqlFile);

        /* Use the following to see DB output number of rows returned
        sprintf(buf,"sql -s %s <  %s 2>&1", G_getenv("DATABASE"), sqlFile);
        */

	if(isatty(1)) strcat (buf," | more");
	system(buf);
	
	unlink(sqlFile) ;

	return 0 ;

}
