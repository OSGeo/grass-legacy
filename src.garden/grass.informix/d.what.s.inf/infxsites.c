#include "gis.h"
#include "infx.h"


infxsites(ktab,ycol, xcol, dist, pts, joinargs, flag)
	struct Option *ktab, *ycol, *xcol, *dist; 
	struct Sql *pts;
	char *joinargs[];
	int flag;
{
	FILE *fp;
	char buf[1024];


        /* Open file to write SQL commands */
        if((fp = fopen(SQL,"w")) == NULL) {
            fprintf(stderr, "File write error on %s\n",SQL);
            exit(-1);
           }




	if ( !(flag)) /******* SQL BOX SOLUTION ***************/
	if (joinargs)  {	/* two table qry with joinkey */
		fprintf(fp,"SELECT DITINCT * FROM %s\n",joinargs[0]);
		fprintf(fp,"WHERE %s in\n",joinargs[1]);
		fprintf(fp,"(select %s from %s where\n",
			joinargs[2],ktab->answer);
		fprintf(fp,"((%s between %f and %f)\n",
			ycol->answer, pts->minY, pts->maxY);
		fprintf(fp,"AND\n");
		fprintf(fp,"(%s between %f and %f)) ) \n",
			xcol->answer, pts->minX, pts->maxX );
	}
	else  {
		fprintf(fp,"SELECT DITINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( (%s between %f and %f  )\n",
			ycol->answer, pts->minY, pts->maxY );
		fprintf(fp,"AND\n");
		fprintf(fp,"(%s between %f and %f ) )\n",
			xcol->answer, pts->minX, pts->maxX );
	}

	else /******** SQL RADIUS SOLUTION ***************/
	if (joinargs)  {	/* two table qry with joinkey */
		fprintf(fp,"SELECT DITINCT * FROM %s\n",joinargs[0]);
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
		fprintf(fp,"SELECT DITINCT * FROM %s\n",ktab->answer);
		fprintf(fp,"WHERE ( ( (%s - %f) * (%s - %f) +\n",
			xcol->answer, pts->centX, xcol->answer, 
			pts->centX);
		fprintf(fp,"(%s - %f) * (%s - %f) ) < %f )\n",
			ycol->answer, pts->centY, ycol->answer,
			pts->centY, pts->rad2 );
	}
	fclose(fp);

        
	fclose (fp);
	sprintf(buf,"isql %s  %s | more\n", G_getenv("DATABASE"), SQL);
	system(buf);

}


