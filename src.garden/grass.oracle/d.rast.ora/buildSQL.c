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

	fprintf(fp, "SET HEA OFF\nSET FEED OFF\n"); 
        if (joinargs != NULL) {
                fprintf(fp, "SELECT DISTINCT %s,%s FROM %s,%s", key, col,
                        table, joinargs[0]);
                fprintf(fp, " WHERE %s = %s ", joinargs[1],
                        joinargs[2]);
                fprintf(fp, " AND %s IS NOT NULL", col);
                fprintf(fp, " ORDER BY %s;\nquit\n", col);
        }
        else {
                fprintf(fp, "SELECT DISTINCT %s,%s FROM %s", key, col, table);
                fprintf(fp, " WHERE %s IS NOT NULL", col);
                fprintf(fp, " ORDER BY %s;\nquit", col);
        }

        fclose(fp);


	return tmpfile_out;
}
