#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include "dbsite.h"
#define TRUE 0
#define FALSE 1

runInfxFile(sqlin, map,  plotargs )
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
    Site *mysite;
  
    mysite=G_site_new_struct(-1,2,0,0);
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


/************************ BEGIN SQL Processing ************************/


	/* Execute SQL command				*/
        sprintf(sysbuf,"isql %s %s > %s",
		G__getenv("DATABASE"),sqlin,SQLOUT);
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


	if (map)	/* if user chose to output sites list */
		fpout=G_fopen_sites_new(map);

	while (!feof(fpin) ) {
            if(fscanf(fpin,"%s%s", buf1,buf2 ) )
               if ((strlen(buf1) >0) && (atoi(buf1) > 0) )    /* don't pass infx col headers */
			if (map) {
				retval = plotsite(atof(buf1),atof(buf2), icon, size);
				if (retval != 0 ) exit(-1);
                                mysite->east=atof(buf1);
                                mysite->north=atof(buf2);
                                G_site_put(fpout,mysite);
			}
			  else	{
					retval = plotsite(atof(buf1),atof(buf2), icon, size);
					if (retval != 0 ) exit(-1);
			  }
	}
   

        fclose(fpin);
        if(map) fclose(fpout);

        R_close_driver();

	/* Remove temporay dot files			*/
		
	unlink(SQL);
	unlink(SQLOUT);
	return(0) ;

}

