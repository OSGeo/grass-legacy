/* runSQL.c - Read user supplied query, execute and draw vectors.
 *
 * INGRES modifications - INGRES requires the query to end with \g (GO)
 *			  to invoke execution (ver5.0 and ver6).
 *			  \g is added to user supplied query.
 *
 *			  The syntax of the execution of the SQL command
 *			  has been modified (ver5.0 and ver6).	
 *
 *			  The processing of the query output has been
 * 			  modified for INGRES. The currently implemented
 *			  version of ingresUnTable applies to INGRES ver5.0.
 *
 * Katarina Johnsson 930416
 */

#include "gis.h"
#include "Vect.h"
#include "ingresUtils.h"
#define TRUE 0
#define FALSE 1

runSQL(sqlin, map, mapset, color )
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
    char *tmpfile_sql;
    char ch;
    int colFlag;	



    tmpfile_out = G_tempfile() ;
    tmpfile_sql = G_tempfile();
    stat = 1 ;
    i = 1;
    line_cat = 0;
    colFlag = 0;

           
        /* Open file for SQL commands */
        if((fp = fopen(sqlin,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",sqlin);
            exit(-1);
           }
/************ Added for INGRES, KJ 930416 *****************************/

	/* Open file for output SQL commands */
        printf("Reading SQL file ...\n");
	if((fpout = fopen(tmpfile_sql,"w")) == NULL) {
		fprintf(stderr, "File write error on temporary file (tmp_sql)\n");
		exit(-1);
	}

	ch = getc(fp);
	while (!feof(fp)) {
		fprintf(fpout, "%c",ch);
		ch = getc(fp);
	}
	fprintf(fpout,"\\g\n");

	fclose(fp);
	fclose(fpout);

/*************End INGRES addition ***************************************/

/************************ BEGIN SQL Processing ************************/

	/* Execute SQL command				*/
        printf("Querying database ...\n");
        sprintf(sysbuf,"sql %s <%s > %s",
		G__getenv("DATABASE"),tmpfile_sql,tmpfile_out);
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
   	printf("Drawing vectors ...\n");
	while (G_getl(buf1, sizeof(buf1), fpin)) {
		ingresUnTable(buf1);
		G_squeeze(buf1);
 		if (*buf1 ==0) continue;
		if (colFlag==0) {
			colFlag=1;
			continue;
     		}
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
	unlink(tmpfile_sql);
        exit(stat);
}

