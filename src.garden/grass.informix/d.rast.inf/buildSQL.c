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


        /* Open file for SQL commands */
        if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temproary file (sql).\n");
            exit(-1);
           }


        if (joinargs != NULL) {
                fprintf(fp, "SELECT DISTINCT %s,%s FROM %s,%s\n", key, col,
                        table, joinargs[0]);
                fprintf(fp, "WHERE %s = %s \n", joinargs[1],
                        joinargs[2]);
                fprintf(fp, "AND %s IS NOT NULL\n", col);
                fprintf(fp, "ORDER BY %s \n", col);
        }
        else {
                fprintf(fp, "SELECT DISTINCT %s,%s FROM %s\n", key, col, table);
                fprintf(fp, "WHERE %s IS NOT NULL\n", col);
                fprintf(fp, "ORDER BY %s \n", col);
        }

        fclose(fp);


        /* Execute SQL command                          */
/*
        sprintf(buf,"isql %s %s > %s",
                G__getenv("DATABASE"),sqlFile,tmpfile_out);
        system(buf);
*/
	return tmpfile_out;
}
