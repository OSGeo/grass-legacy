#include "gis.h"
#include "infx.h"


buildInfxQry(ktab,ycol, xcol, dist, pts, joinargs)
	struct Option *ktab, *ycol, *xcol, *dist; 
	struct Sql *pts;
	char *joinargs[];
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

	if (joinargs)  {	/* two table qry with joinkey */
/* original
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",joinargs[0]);
		fprintf(fp,"WHERE %s in\n",joinargs[1]);
		fprintf(fp,"(select %s from %s where\n",
			joinargs[2],ktab->answer);
		fprintf(fp,"( ( (%s - %f) * (%s - %f) +\n",
			xcol->answer, pts->centX, xcol->answer, 
			pts->centX);
		fprintf(fp,"(%s - %f) * (%s - %f) ) ) < %f )\n",
			ycol->answer, pts->centY, ycol->answer,
			pts->centY, pts->rad2 );
*/
/* postgres query by J.Soimasuo */
		fprintf(fp,"SELECT unique (%s.all) \n",
			joinargs[0]);
		fprintf(fp,"where %s = %s \n",
			joinargs[1], joinargs[2]);
		fprintf(fp,"and ( ( (%s.%s - %f) * (%s.%s - %f) +\n",
			ktab->answer,xcol->answer, pts->centX, 
			ktab->answer,xcol->answer, pts->centX);
		fprintf(fp,"(%s.%s - %f) * (%s.%s - %f) ) ) < %f \n",
			ktab->answer,ycol->answer, pts->centY, 
			ktab->answer,ycol->answer,pts->centY, pts->rad2 );

               }

	else  {
/* original
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( ( (%s - %f) * (%s - %f) +\n",
			xcol->answer, pts->centX, xcol->answer, 
			pts->centX);
		fprintf(fp,"(%s - %f) * (%s - %f) ) < %f )\n",
			ycol->answer, pts->centY, ycol->answer,
			pts->centY, pts->rad2 );
*/
/* Postquel query by J.Soimasuo */
	  fprintf(fp,"SELECT unique (%s.all) \n",
		ktab->answer);
	  fprintf(fp,"where ( ( (%s.%s - %f) * (%s.%s - %f) +\n",
		ktab->answer,xcol->answer, pts->centX, 
		ktab->answer,xcol->answer, pts->centX);
	  fprintf(fp,"(%s.%s - %f) * (%s.%s - %f) ) < %f )\n",
		ktab->answer,ycol->answer, pts->centY, 
		ktab->answer,ycol->answer,pts->centY, pts->rad2 );  
	}
	fclose(fp);

/* original
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


