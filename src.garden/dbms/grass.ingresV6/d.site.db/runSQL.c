/* runSQL - Read user supplied SQL query, execute query and display result
 *
 * INGRES modifications - 'ingresUnTable' added to read output.
 *			  Header file 'ingresUtils.h' added.
 *
 *			  '\g' added to query.
 *
 *			  Syntax for exeution of SQL command modified.
 *
 * Katarina Johnsson, 930421
 */

#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include "dbsite.h"
#include "ingresUtils.h" 
#define TRUE 0
#define FALSE 1

runSQL(sqlin, map,  plotargs )
  char *sqlin, *map, *plotargs[];

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
    char buf[1024];		/* temp buffer for read Ingres output */
    char *tmpfile_sql;		/* added for INGRES */
    char ch;			/* added for INGRES */


    i = 1;
    retval  = 0 ;

	/* If user didn't specify ... then go with defaults 	*/
	color = D_translate_color(plotargs[0]);
	icon = plotargs[1];
	size = atoi(plotargs[2]);

           
        /* Open file for SQL commands */
        if((fp = fopen(sqlin,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",sqlin);
            exit(-1);
           }

/***** Added for INGRES, KJ 930421 **********/

	printf("Reading SQL query ...\n");

	tmpfile_sql = G_tempfile();

	/* Open file for output SQL command */
	if((fpout = fopen(tmpfile_sql, "w")) == NULL) {
		fprintf(stderr, "File write error on temporary file \n");
		exit(-1);
	}

	ch = getc(fp);
	while (!feof(fp)) {
		fprintf(fpout, "%c",ch);
		ch = getc(fp);
	}
	fprintf(fpout, "\\g\n");

	fclose(fp);
	fclose(fpout);

	/* Check output OK */
	if((fp = fopen(tmpfile_sql, "r")) == NULL) {
		fprintf(stderr, "File read error on temporary file\n");
		exit(-1);
	}
/****************** end INGRES addition *****************/


/************************ BEGIN SQL Processing ************************/


	/* Execute SQL command				*/

	printf("Querying database ...\n");
        sprintf(sysbuf,"sql -s %s < %s > %s",
		G__getenv("DATABASE"),tmpfile_sql,SQLOUT);
        system(sysbuf);
        
	/* Open file containing output from SQL command	*/
	if((fpin = fopen(SQLOUT,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",SQLOUT);
	    exit(-1);
           }

/************************ END SQL Processing ************************/




/************** use SQL output COORDS & plot points ****************/

	/* Initialize graphics stuff */

	R_open_driver();
	D_setup() ;
	R_standard_color(color);

	printf("Drawing sites ...\n");
	if (map)	/* if user chose to output sites list */
		fpout=G_fopen_sites_new(map);

	while (!feof(fpin) ) {
	    if(G_getl(buf, sizeof(buf),fpin)) {
	    G_squeeze(buf);
	    ingresUnTable(buf);
	    if(*buf==0) continue;
	    if(sscanf(buf,"%s%s", buf1, buf2)) {
               if ((strlen(buf1) >0) && (atoi(buf1) > 0) ) {   /* don't pass infx col headers */
			if (map) {
				retval = plotsite(atof(buf1),atof(buf2), icon, size);
				if (retval != 0 ) exit(-1);
				G_put_site(fpout,atof(buf1),atof(buf2),"");
			}
			  else	{
					retval = plotsite(atof(buf1),atof(buf2), icon, size);
					if (retval != 0 ) exit(-1);
			  }
			  }
		}
		}
	}
   

        fclose(fpin);
        if(map) fclose(fpout);

        R_close_driver();

	/* Remove temporay dot files			*/
		
	unlink(SQL);
	unlink(SQLOUT);
	unlink(tmpfile_sql);
	return(0) ;

}

