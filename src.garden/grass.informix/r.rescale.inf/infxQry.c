#include "gis.h"
#include <stdlib.h>
#include "dbrescale.h"
#include "infx.h"
#define LINE 80
#define TRUE 0
#define FALSE 1

infxQry(tab,key,col, cats, raster,rastout,joinargs, mapset)
  char *tab, *key, *col, *cats,*raster, *rastout, *joinargs[], mapset;
  {
    FILE *fpin, *fpout;
    char buf[1024];
    char sysbuf[1024];
    char minbuf[20], maxbuf[20];
    char colbuf[20], keybuf[20];
    int mincol, maxcol, interval, keyval, colval;
    int curmax, oldmax, curcat ;
    char *tmpfile_out;
    char sqlFile[100];
    char rules[100];


    sprintf(sqlFile,"/tmp/%d.sql",getpid() );

    tmpfile_out = G_tempfile();

           
	if((fpout = fopen(sqlFile,"w")) == NULL) {   /* create select for MIN/MAXon col  */
            fprintf(stderr, "File write error on rules file\n");
	    exit(-1);
           }

        fprintf(fpout,MIN_MAX,col, col, tab);
	fclose(fpout);

	sprintf(sysbuf,"isql %s  < %s > %s\n", G_getenv("DATABASE"), sqlFile, tmpfile_out);
        
        system(sysbuf);

	if((fpin = fopen(tmpfile_out,"r")) == NULL) {   /* Now read min/max val for col */
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }
	


        /* Read SQL output and get MIN, MAX & interval rescale range for column */
	while(G_getl(buf, sizeof(buf), fpin) ) {
		G_squeeze(buf);
		if (*buf == 0) continue;

		if(sscanf(buf,"%d %d", &mincol, &maxcol) != 2 ) continue;
		if (mincol == 0) mincol=1 ; /* to seperate 1st interval from no data */
		if ( (maxcol-mincol) % atoi(cats) == 0 ) 
			interval =  (maxcol-mincol) / atoi(cats);
		  else
			interval =  (maxcol-mincol) / atoi(cats) + 1;
		break;
	}

	fclose(fpin) ;


	/* Create SQL cmds file to select column values 	*/

	if((fpout = fopen(sqlFile,"w")) == NULL) {   
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

        if (joinargs != NULL) {
                fprintf(fpout, "SELECT DISTINCT %s,%s FROM %s,%s\n", key, col,
                        tab, joinargs[0]);
                fprintf(fpout, "WHERE %s = %s \n", joinargs[1],
                        joinargs[2]);
                fprintf(fpout, "AND %s IS NOT NULL\n", col);
                fprintf(fpout, "ORDER BY %s \n", col);
        }
	 else {
		fprintf(fpout,"SELECT DISTINCT %s, %s FROM %s\n", key,col, tab);
		fprintf(fpout,"WHERE %s IS NOT NULL\n",col) ;
		fprintf(fpout,"ORDER BY %s\n", col);
	}

	fclose(fpout);

	sprintf(sysbuf,"isql %s < %s > %s\n", G_getenv("DATABASE"), sqlFile, "tmpfile_out");
	system(sysbuf);


	/* Generate values for column sorted (ascending) */

	if((fpin = fopen("tmpfile_out","r")) == NULL) {   /* Now read unique vals for col */
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }


    	sprintf(rules,"/tmp/%d.sql",getpid() );
	if((fpout = fopen(rules,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary files\n");
	    exit(-1);
           }

	oldmax = 1;
        curmax = mincol + interval ;
	curcat = 1;

        while(G_getl(buf, sizeof(buf), fpin) ) {
                G_squeeze(buf);
                if (*buf == 0) continue;

                if(sscanf(buf,"%d %d", &colval, &keyval) != 2 ) continue;
		if (keybuf == 0)  
			fprintf(fpout,"0=0 %s is 0\n", col);
		else {
			if(keyval <= curmax) {
                        	fprintf(fpout,"%d=%d %s [%d-%d]\n",colval,curcat,col,oldmax,curmax);
                        	fprintf(stderr,"%d=%d %s [%d-%d]\n",colval,curcat,col,oldmax,curmax);
				}
			else {   
				oldmax = curmax + 1;
				curmax = oldmax + interval + 1 ;
				curcat++ ;
                        	fprintf(fpout,"%d=%d %s [%d-%d]\n",colval,curcat,col,oldmax,curmax);
                        	fprintf(stderr,"%d=%d %s [%d-%d]\n",colval,curcat,col,oldmax,curmax);
			}
		}


        }

	fclose(fpin);
	fclose(fpout);

	sprintf(sysbuf,"r.reclass input=%s output=%s <%s\n", raster, rastout, rules);
	system(sysbuf);

	unlink(sqlFile);
	unlink(tmpfile_out);
	unlink(rules);
 
	return 0 ;

}
