
#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include "dbsite.h"
#define TRUE 0
#define FALSE 1

buildInfxQry(table, xy, where, map, joinargs, plotargs )
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
    Site *mysite;
  
    mysite=G_site_new_struct(-1,2,0,0);
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


/************************ BEGIN SQL Processing ************************/

        if (joinargs != NULL) { 
		fprintf(fp, "SELECT %s,%s FROM %s,%s\n", xy[0],  
			xy[1], table, joinargs[0]);
		fprintf(fp, "WHERE %s \n", where);
		fprintf(fp, "AND %s=%s \n", joinargs[1], joinargs[2]);
		fprintf(fp, "AND %s IS NOT NULL AND %s IS NOT NULL\n", 
			xy[0], xy[1]);
		fprintf(fp, "ORDER BY %s, %s \n", xy[0], xy[1]);
	}
	else {		   
		fprintf(fp, "SELECT %s,%s FROM %s\n", xy[0],xy[1], table);
		fprintf(fp, "WHERE %s \n", where);
		fprintf(fp, "AND %s IS NOT NULL  AND %s IS NOT NULL\n", 
			 xy[0], xy[1]);
		fprintf(fp, "ORDER BY %s, %s \n", xy[0], xy[1]);
	}

	fclose(fp);


	/* Execute SQL command				*/
        sprintf(sysbuf,"isql %s %s > %s",
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

	while (!feof(fpin) ) {
            if(fscanf(fpin,"%s%s", buf1,buf2 ) )
               if ((strlen(buf1) >0) && (strncmp(G_squeeze(buf1),xy[0],strlen(xy[0])) != 0) ) 
			if (map) {
				retval = plotsite(atof(buf1),atof(buf2), icon, size);
                                mysite->east=atof(buf1);
                                mysite->north=atof(buf2);
                                G_site_put(fpout,mysite);
				if(retval != 0) exit(-1);
			}
			  else {
				retval = plotsite(atof(buf1),atof(buf2), icon, size);
				if(retval != 0) exit(-1);
			   }
	}
   

        fclose(fpin);
        fclose(fpout);

        R_close_driver();

	/* Remove temporay dot files			*/
		
	unlink(SQL);
	unlink(SQLOUT);

	return(0);
}

