#include "gis.h"


buildInfxQry(ktab,keycat, joinargs, curcat)
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
            fprintf(stderr, "File write error on temproary file (sql).\n");
            exit(-1);
           }


	if (joinargs)  {	/* two table qry with joinkey */
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",joinargs[0]);
		fprintf(fp,"WHERE %s in\n",joinargs[1]);
		fprintf(fp,"(select %s from %s \n",
			joinargs[2],ktab->answer);
		fprintf(fp,"WHERE ( %s = %d ) )\n",
			keycat->answer, curcat);

	}
	else  {
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( %s = %d )\n",
			keycat->answer, curcat);
	}

        
	fclose (fp);
        sprintf(buf,"isql %s  %s 2>/dev/null", G_getenv("DATABASE"), sqlFile);

        /* Use the following to see DB output number of rows returned
        sprintf(buf,"isql %s  %s 2>&1", G_getenv("DATABASE"), sqlFile);
        */

	if(isatty(1)) strcat (buf," | more");
	system(buf);
	unlink(sqlFile);

	return 0;
}
