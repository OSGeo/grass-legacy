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
            fprintf(stderr, "File write error on temprorary file (sql).\n");
            exit(-1);
           }


	if (joinargs)  {	/* two table qry with joinkey */
/*   ORIGINAL
	        fprintf(fp,"SELECT DISTINCT * FROM %s\n",joinargs[0]);
		fprintf(fp,"WHERE %s in\n",joinargs[1]);
		fprintf(fp,"(select %s from %s \n",
			joinargs[2],ktab->answer);
		fprintf(fp,"WHERE ( %s = %d ) )\n",
			keycat->answer, curcat);
*/
/* postquel-query by J.Soimasuo*/	  
	  fprintf(fp, "SELECT unique (%s.all)\n", joinargs[0]);
	  fprintf(fp, "where %s = %s \n", joinargs[1],joinargs[2]);
	  fprintf(fp, "and %s.%s = %d \n", ktab->answer,keycat->answer, curcat);
	}
	else  {
/*   ORIGINAL	  
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( %s = %d )\n",
			keycat->answer, curcat);
*/
/* postquel-query by J.Soimasuo */
	  fprintf(fp, "SELECT unique (%s.all)\n", ktab->answer);
	  fprintf(fp, "where %s.%s = %d \n", ktab->answer,keycat->answer, curcat);
        }

        
	fclose (fp);
/*	
        sprintf(buf,"isql %s  %s 2>/dev/null", G_getenv("DATABASE"), sqlFile);
*/
        sprintf(buf,"psql -q -c \" ` cat %s ` \" %s ",
                 sqlFile,G__getenv("PG_DBASE"));

        /* Use the following to see DB output number of rows returned
        sprintf(buf,"isql %s  %s 2>&1", G_getenv("DATABASE"), sqlFile);
        */

	if(isatty(1)) strcat (buf," | more");
	system(buf);

	unlink(sqlFile) ;

	return 0 ;
}


