/* sqlQry - Do it!
 *
 * INGRES modifications - 

	Syntax for execution of SQL command modified.

	ingresUnTable added to remove table frames etc from SQL output.
	Currently implemented version of ingresUnTable is valid 
	for INGRES ver5.0 only!

	SQL syntax for MIN_MAX (dbrescale.h and sqlQry) modified.

	SQL syntax for Selection (sqlQry) modified. 
	IS NOT NULL is commented out to 
	work with INGRES 5.0. 
	'\g' added to select statement.

 * Bug Fixes       - Ensure correct table is input to MIN_MAX.
	   	     
		     1st argument when writing reclass rules is 'keyval' and
		     not 'colval'.

		     Condition to write reclass rules is '(colval <= curmax)'
		     and not '(keyval <= curmax)'. 

		     Variable 'oldmax' initialized to 'mincol' and not '1'.

		     Variable 'curmax' is set to 'oldmax + interval' and not
		     'oldmax + interval + 1' when the category is incremented
		     for writing to reclass rules file.

		     The maximum category value for the last interval is set
		     to 'maxcol' and not to 'curmax + interval'.
	   
		     If the MIN column value is '0' it is still included in
		     the first interval, ie 'mincol' is not changed to 1. 
		     '0' is a value in the reclass column and should not be
		     treated as 'no data'.

 * Improvements       - The definition of intervals has been improved to 
			avoid that the last interval differs a lot in size
			from the others. This would be the case when the    
			integer division gives a large remainder. With the
		  	improved implementation, the difference can at most be
			half the size of the interval. 

 * Remaining bugs - r.reclass.db can only operate on integer columns. This 
		 should be improved so that float is accepted.
 
 * Katarina Johnsson 930417
 */

#include "gis.h"
#include <stdlib.h>
#include "dbrescale.h"
#include "ingresUtils.h"
#include "infx.h"
#define LINE 80
#define TRUE 0
#define FALSE 1

sqlQry(tab,key,col, cats, raster,rastout,joinargs, mapset)
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
    char tmpbuf[100];


    sprintf(sqlFile,"/tmp/%d.sql",getpid() );

    tmpfile_out = G_tempfile();

           
	if((fpout = fopen(sqlFile,"w")) == NULL) {   /* create select for MIN/MAXon col  */
            fprintf(stderr, "File write error on rules file\n");
	    exit(-1);
           }

	/* if statement added to get correct table for MIN_MAX. KJ 930417 */
	if (joinargs != 0) strcpy(tmpbuf,joinargs[0]) ;
	else strcpy(tmpbuf,tab);
	printf("%s\n",tmpbuf);
        fprintf(fpout,MIN_MAX,col, col, tmpbuf, "\\g\n");
	fclose(fpout);

	/* Modified for INGRES ver 5.0 and ver6. KJ 930417 */
 	printf("Getting MIN and MAX for column %s ...\n", col);
	sprintf(sysbuf,"sql %s  < %s > %s", G_getenv("DATABASE"), sqlFile, tmpfile_out);
        
        system(sysbuf);


	if((fpin = fopen(tmpfile_out,"r")) == NULL) {   /* Now read min/max val for col */
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }
	


        /* Read SQL output and get MIN, MAX & interval rescale range for column */
	while(G_getl(buf, sizeof(buf), fpin) ) {
		ingresUnTable(buf);
		G_squeeze(buf);
		printf("BUF= %s\n",buf);
		if (*buf == 0) continue;

		if(sscanf(buf,"%d %d", &mincol, &maxcol) != 2 ) continue;
/*
		if (mincol == 0) mincol=1 ;  to seperate 1st interval from no data */
		if ( (maxcol-mincol) % atoi(cats) == 0 ) 
			interval =  (maxcol-mincol) / atoi(cats);
		  else {
			if ((maxcol-mincol) % atoi(cats) < 
			    (atoi(cats) - atoi(cats)/2))
				interval=(maxcol-mincol) / atoi(cats);
			else
				interval =  (maxcol-mincol) / atoi(cats) + 1;
		  }
		printf("MIN= %d, MAX= %d, interval= %d\n",mincol, maxcol,interval);
		break;
	}

	fclose(fpin) ;


	/* Create SQL cmds file to select column values 	*/

	if((fpout = fopen(sqlFile,"w")) == NULL) {   
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

	/* Syntax modified for INGRES. KJ 930417 */
        if (joinargs != NULL) {
                fprintf(fpout, "SELECT DISTINCT out=%s.%s,%s.%s FROM %s,%s\n",  			joinargs[0], col, tab, key,joinargs[0], tab);
                fprintf(fpout, "WHERE %s.%s = %s.%s \n", joinargs[0],           			joinargs[1], tab, joinargs[2]);
    		/* INGRES ver5.0 does not support IS NOT NULL
                fprintf(fpout, "AND %s IS NOT NULL\n", col);
		*/
                fprintf(fpout, "ORDER BY out \n");
		fprintf(fpout, "\\g\n");
        }
	 else {
		fprintf(fpout,"SELECT DISTINCT %s, %s FROM %s\n", col, key, tab);
		/* INGRES ver5.0 does not support IS NOT NULL 
		fprintf(fpout,"WHERE %s IS NOT NULL\n",col) ;
		*/
		fprintf(fpout,"ORDER BY %s\n", col);
		fprintf(fpout,"\\g\n");
	}

	fclose(fpout);

	/* Modified for INGRES ver5.0 and ver6. KJ 930417 */

	printf("Querying database ... \n");
	sprintf(sysbuf,"sql %s < %s > %s", G_getenv("DATABASE"), sqlFile, tmpfile_out);
	system(sysbuf);

	sprintf(sysbuf, "sql %s < %s", G_getenv("DATABASE"), sqlFile);
	system(sysbuf);

	/* Generate values for column sorted (ascending) */

	if((fpin = fopen(tmpfile_out,"r")) == NULL) {   /* Now read unique vals for col */
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }


    	sprintf(rules,"/tmp/%d.sql",getpid() );
	if((fpout = fopen(rules,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary files\n");
	    exit(-1);
           }

	oldmax = mincol;
        curmax = mincol + interval ;
	curcat = 1;

	printf("Building GRASS rescale rules ...\n");

        while(G_getl(buf, sizeof(buf), fpin) ) {
		ingresUnTable(buf);
                G_squeeze(buf);
                if (*buf == 0) continue;

                if(sscanf(buf,"%d %d", &colval, &keyval) != 2 ) continue;
		if (keybuf == 0) 
			fprintf(fpout,"0=0 %s is 0\n", col);
		else {
			if(colval <= curmax) {
                        	fprintf(fpout,"%d=%d %s [%d-%d]\n",keyval,curcat,col,oldmax,curmax);
				printf("%d=%d %s [%d-%d]\n",keyval,curcat,col,oldmax,curmax);
			}
			else {   
				oldmax = curmax + 1;
				curmax = oldmax + interval ;
				if (curmax > maxcol) curmax=maxcol; /*last cat*/
				curcat++ ;
                        	fprintf(fpout,"%d=%d %s [%d-%d]\n",keyval,curcat,col,oldmax,curmax);
				printf("%d=%d %s [%d-%d]\n",keyval,curcat,col,oldmax,curmax);
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

	printf("\nDone\nRun 'd.rast %s' to display rescale map.\n", rastout); 
	return 0 ;

}
