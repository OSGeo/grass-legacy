/* buildSQL.c - Build and execute the SQL query
 *
 * INGRES modifications - The SQL syntax has been modified.
 *			  Correct INGRES ver5.0 and ver.6 syntax:
 *
 *			  (single table)
 *			  SELECT DISTINCT key,col from table
 *			  ORDER BY col;
 *
 *			  (join)
 *			  SELECT DISTINCT table1.key, out=table2.col
 *			  from table1, table2
 *			  WHERE table1.pkey=table2.tabkey
 *			  ORDER BY out;
 *
 *			  INGRES ver5.0 does not support 'IS NOT NULL'
 *			  INGERS ver6 supports 'IS NOT NULL' and these
 *			  lines can be de-commented for ver6.
 *
 *			  The syntax of the execution of the SQL command
 *			  has been modified (INGRES ver5 and ver6).
 *
 *			  Processing of output from SQL query modified
 * 			  for INGRES. Currently implemented version of 
 *			  ingresUnTable applies to INGRES version 6.
 *
 * Improvements - No improvements have been implemented in this routine.
 *
 * Katarina Johnsson 930416
 */

#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "ingresUtils.h"
#define TRUE 0
#define FALSE 1

buildSQL(key, where, table, map, joinargs, mapset, color)
  char *key,*where, *table, *map, *joinargs[], *mapset;
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
    char sqlFile[100];
    char *tmpfile_out;
    int colFlag;


    sprintf(sqlFile,"/tmp/%d.sql",getpid() );
    tmpfile_out = G_tempfile() ;
    stat = 1;
    i = 1;
    line_cat = 0;
    colFlag=0;

           
        /* Open file for SQL commands */
        if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (sql).\n");
            exit(-1);
           }

	/* Build the query. Modified for INGRES  KJ 930416 */
        printf("Building the SQL query ...\n");
        if (joinargs != NULL) { 
		fprintf(fp, "SELECT DISTINCT out=%s.%s FROM %s,%s\n", table,key,  
			table, joinargs[0]);
		fprintf(fp, "WHERE %s \n", where);
		fprintf(fp, "AND %s.%s=%s.%s\n", table,joinargs[2],joinargs[0],joinargs[1]); 
		fprintf(fp, "AND %s.%s IS NOT NULL\n",table, key); 
		fprintf(fp, "ORDER BY out \n"); 
		fprintf(fp, "\\g\n");
	}
	else {		   
		fprintf(fp, "SELECT DISTINCT %s FROM %s\n", key, table);
		fprintf(fp, "WHERE %s \n", where);
		fprintf(fp, "AND %s IS NOT NULL\n", key);
		fprintf(fp, "ORDER BY %s \n", key);
		fprintf(fp, "\\g\n");
	}

	fclose(fp);


	/* Execute SQL command				*/
        printf("Querying database ...\n");
        sprintf(sysbuf,"sql %s <%s > %s",
		G__getenv("DATABASE"),sqlFile,tmpfile_out);
        system(sysbuf);


        /* Open file containing output from SQL command	*/
	if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql output)\n");
	    exit(-1);
           }


/* Now that the SQL has output CAT value(s) plot vectors with same cat vals */

	/* Initialize graphics stuff */

	R_open_driver();
	D_setup(0) ;
	R_standard_color(color);
	Points = Vect_new_line_struct();  
	if (2 > Vect_open_old (&P_map, map, mapset))
	{
		return -1;
	 }
	 build_lookup_tables(&P_map);

	/* Read SQL output and draw vectors */
        printf("Drawing vectors ...\n");
	while(G_getl(buf1, sizeof(buf1), fpin)) {
		ingresUnTable(buf1);
		G_squeeze(buf1);
		if (*buf1 == 0) continue;
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
	unlink(sqlFile);
	unlink(tmpfile_out);
		
        exit(stat);
}

