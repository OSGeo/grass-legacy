#include "gis.h"
#include "Vect.h"
#define TRUE 0
#define FALSE 1

runInfxFile(sqlin, map, mapset, color )
  char *sqlin, *map, *mapset;
  int color ;
  {
    FILE *fp, *fpin, *fpout;
    int i;
    int line_cat;		/* vector cat values from db 	*/ 
    int stat;			/* return value from plot2 	*/
    char sysbuf[1024];		/* buffer to issue system cmd 	*/
    char buf1[1024];		/* value in key column		*/
    struct line_pnts *Points ;
    struct Map_info P_map;
    char *tmpfile_out;



    tmpfile_out = G_tempfile() ;
    stat = 1 ;
    i = 1;
    line_cat = 0;

           
        /* Open file for SQL commands */
        if((fp = fopen(sqlin,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",sqlin);
            exit(-1);
           }

/************************ BEGIN SQL Processing ************************/

	/* Execute SQL command				*/
        sprintf(sysbuf,"isql %s %s > %s",
		G__getenv("DATABASE"),sqlin,tmpfile_out);
        system(sysbuf);

        /* Open file containing output from SQL command	*/
	if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql output)\n");
	    exit(-1);
           }

/************************ END SQL Processing ************************/


/* Now that the SQL has output CAT value(s) plot vectors with same cat vals */

	/* Initialize graphics stuff */

	R_open_driver();
	D_setup() ;
	R_standard_color(color);
	Points = Vect_new_line_struct();
        if (2 > Vect_open_old (&P_map, map, mapset))
        {
                return -1;
         }
	 build_lookup_tables (&P_map);



	/* Read SQL output and draw vectors */
	while (!feof(fpin) ) {
            if(fscanf(fpin,"%s", buf1 ) ) 
               if (strlen(buf1) >0)  
			if (atoi(buf1) != line_cat) {
				line_cat = atoi(buf1) ;
                                stat = plotCat(map,mapset,Points,line_cat, &P_map);
                        } 
	}
   

        fclose(fpin);
        Vect_destroy_line_struct (Points);
        Vect_close(&P_map) ;

        R_close_driver();



	/* Remove temporay dot files			*/
		
	unlink(tmpfile_out);
        exit(stat);
}

