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
	}

	else  {
		fprintf(fp,"SELECT DISTINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( ( (%s - %f) * (%s - %f) +\n",
			xcol->answer, pts->centX, xcol->answer, 
			pts->centX);
		fprintf(fp,"(%s - %f) * (%s - %f) ) < %f )\n",
			ycol->answer, pts->centY, ycol->answer,
			pts->centY, pts->rad2 );
	}
	fclose(fp);

	sprintf(buf,"isql %s  %s 2>/dev/null", G_getenv("DATABASE"), sqlFile);

	/* Use the following to see DB output number of rows returned 
	sprintf(buf,"isql %s  %s 2>&1", G_getenv("DATABASE"), sqlFile);
	*/

	if(isatty(1)) strcat (buf," | more");
	system(buf);

	unlink(sqlFile) ;

	return 0 ;

}


