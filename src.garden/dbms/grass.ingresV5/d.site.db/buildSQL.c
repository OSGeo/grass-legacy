/* buildSQL - build SQL query, execute and display output
 *
 * INGRES modifications - Header file ingresUtils.h added.
 *			  Routine ingresUnTable added to read SQL output.
 *
 *			  Syntax for execution of SQL command modified.
 *
 *			  Syntax for SQL query modified, and '\g' added.
 *
 *
 * Katarina Johnsson 930419
 */
#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include "dbsite.h"
#include "ingresUtils.h"
#define TRUE 0
#define FALSE 1

buildSQL(table, xy, where, map, joinargs, plotargs )
  char *table,*xy[], *where, *map, *joinargs[], *plotargs[];

  {
    FILE *fp, *fpin, *fpout;
    int i;
    int retval;
    int color;			/* Color for site plots 	*/ 
    int size;			/* Size for site plots	 	*/
    char *icon;			/* Icon used in plot		*/
    char sysbuf[1024];		/* buffer to issue system cmd 	*/
    char buf1[1024];		/* value in Xcol column		*/
    char buf2[1024];		/* value in Ycol column		*/
    char buf[1024];		/* temp buffer for Ingres output */


    i = 1;
    retval = 0;

	color = D_translate_color(plotargs[0]);
	icon = plotargs[1];
	size = atoi(plotargs[2]);

           
        /* Open file for SQL commands */
        if((fp = fopen(SQL,"w")) == NULL) {
            fprintf(stderr, "File write error on %s\n",SQL);
            exit(-1);
           }


/********* BEGIN SQL Processing, modified for INGRES  ********************/
/* Statement 'IS NOT NULL is not supported by INGRES ver5.0. These lines
   can be de-commented for INGRES ver.6. All other syntax changes work
   for both versions of INGRES. KJ 930419.
*/

	printf("Building SQL query ...\n");

        if (joinargs != NULL) { 
		fprintf(fp, "SELECT DISTINCT xc=%s.%s,yc=%s.%s\n",
			table, xy[0], table, xy[1]); 
		fprintf(fp,"FROM %s,%s\n", table, joinargs[0]);
		fprintf(fp, "WHERE %s \n", where);
		fprintf(fp, "AND %s.%s=%s.%s \n", 
			joinargs[0], joinargs[1], table, joinargs[2]);
		/* Doen't work with INGRES ver5.0, KJ93021
		fprintf(fp, "AND %s IS NOT NULL AND %s IS NOT NULL\n", 
			xy[0], xy[1]);
 		*/
		fprintf(fp, "ORDER BY xc, yc \n");
		fprintf(fp, "\\g\n");
	}
	else {		   
		fprintf(fp, "SELECT DISTINCT %s,%s \n", xy[0], xy[1]); 
		fprintf(fp, "FROM %s\n", table);
		fprintf(fp, "WHERE %s \n", where);
		/* Doesn't work with INGRES 5.0, KJ 930421  
		fprintf(fp, "AND %s IS NOT NULL  AND %s IS NOT NULL\n", 
			 xy[0], xy[1]);
     		*/
		fprintf(fp, "ORDER BY %s, %s \n", xy[0], xy[1]);
		fprintf(fp, "\\g\n");
	}

	fclose(fp);


	/* Execute SQL command				*/

	printf("Querying database ...\n");
        sprintf(sysbuf,"sql -s %s < %s > %s",
		G__getenv("DATABASE"),SQL,SQLOUT);
        system(sysbuf);

        /* Open file containing output from SQL command	*/
	if((fpin = fopen(SQLOUT,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",SQLOUT);
	    exit(-1);
           }

/************************ END SQL Processing ************************/




/* Now that the SQL has output COORDS plot points 	*/

	/* Initialize graphics stuff */

	R_open_driver();
	D_setup() ;
	R_standard_color(color);


	/* Read SQL output and draw vectors */

	if (map)	/* if user chose to output sites list */
		fpout=G_fopen_sites_new(map);

	printf("Displaying sites ...\n");
	while (!feof(fpin) ) {
	    if(G_getl(buf,sizeof(buf),fpin)) {
		G_squeeze(buf);
		ingresUnTable(buf);
		if(*buf==0) continue;
		if(sscanf(buf,"%s%s", buf1,buf2))
               if ((strlen(buf1) >0) && (strncmp(G_squeeze(buf1),xy[0],strlen(xy[0])) != 0) ) {
			if (map) {
				retval = plotsite(atof(buf1),atof(buf2), icon, size);
				G_put_site(fpout,atof(buf1),atof(buf2),"");
				if(retval != 0) exit(-1);
			}
			  else {
				retval = plotsite(atof(buf1),atof(buf2), icon, size);
				if(retval != 0) exit(-1);
			   }
		}
		}
	}
   

        fclose(fpin);
        if (map) fclose(fpout);

        R_close_driver();

	/* Remove temporay dot files			*/
		
	unlink(SQL);
	unlink(SQLOUT);

	return(0);
}

