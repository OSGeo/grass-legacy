

#include "gis.h"


ingres_del_key(sqlFile)
	char *sqlFile;
{
	FILE *fp;


        /* Open file to edit SQL commands */
        if((fp = fopen(sqlFile,"a")) == NULL) {
            fprintf(stderr, "File append error on temporary file (sql).\n");
            exit(-1);
           }

	fprintf(fp,"WHERE (%s = %d) \n", key, cur)  ;
	fprintf(fp,"\\g\n");
        
	fclose (fp);

	return 0;
}
