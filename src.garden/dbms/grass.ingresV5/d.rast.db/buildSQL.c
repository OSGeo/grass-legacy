/* buildSQL.c - Build the SQL query
 *
 * INGRES modifications - The SQL syntax has been modified.
 *			  Correct INGRES ver5.0 and ver6 syntax:
 *	
 *                        (single table)
 *                        SELECT DISTINCT key,col from table
 *			  ORDER BY col;
 *
 *                        (join)
 *			  SELECT DISTINCT table1.key, out=table2.col      
 * 			  from table1, table2
 *            		  WHERE table1.pkey=table2.tabkey
 *			  ORDER BY out;
 *
 *			  Ingres ver5.0 does not support 'IS NOT NULL'
 *			  Ingres ver6 supports 'IS NOT NULL' and these
 *			  lines can be de-commented for ver6.
 *
 * Improvements - No improvements have been implemented in this routine
 *
 * KJ 930411
 */

#include "gis.h"
#include "dbrast.h"

char *buildSQL(key, col, table, input, output, joinargs )
  char *key,*col, *table, *input, *output, *joinargs[];
  {
    FILE *fp, *fpin, *fpout;
    char buf[1024];             /* value in key column          */
    int outFlag;
    char sqlFile[100];
    char *tmpfile_out;

    sprintf(sqlFile,"/tmp/%d.sql",getpid() );
    tmpfile_out = sqlFile ;


	printf("Building the SQL query ... \n");
        /* Open file for SQL commands */
        if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temproary file (sql).\n");
            exit(-1);
           }

	/* SQL syntax modified for INGRES ver 5.0. KJ 930411 */
        if (joinargs != NULL) {
                fprintf(fp, "SELECT DISTINCT %s.%s,out=%s.%s FROM %s,%s\n",
                        table, key, joinargs[0], col, table, joinargs[0]);
                fprintf(fp, "WHERE %s.%s = %s.%s\n", joinargs[0], joinargs[1],
                        table, joinargs[2]);

/* IS NOT NULL works with INGRES ver.6 but not with INGRES ver.5.0, KJ 930411  
                fprintf(fp, "AND %s IS NOT NULL\n", col);
*/
                fprintf(fp, "ORDER BY out \n");
        }
        else {
                fprintf(fp, "SELECT DISTINCT %s,%s FROM %s\n", key, col, table);

/* IS NOT NULL doesn't work with INGRES VER 5.0, KJ 930411  
                fprintf(fp, "WHERE %s IS NOT NULL\n", col);
*/
                fprintf(fp, "ORDER BY %s \n", col);
	     }

        fclose(fp);


        /* Execute SQL command (this is done in runSQL) */
/*
        sprintf(buf,"isql %s %s > %s",
                G__getenv("DATABASE"),sqlFile,tmpfile_out);
        system(buf);
*/
	return strdup(tmpfile_out);
}
