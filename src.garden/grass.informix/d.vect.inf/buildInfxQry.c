#include "gis.h"
#include "Vect.h"
#define TRUE 0
#define FALSE 1

buildInfxQry(key, where, table, map, joinargs, mapset, color)
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


    sprintf(sqlFile,"/tmp/%d.sql",getpid() );
    tmpfile_out = G_tempfile() ;
    stat = 1;
    i = 1;
    line_cat = 0;

           
        /* Open file for SQL commands */
        if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (sql).\n");
            exit(-1);
           }


        if (joinargs != NULL) { 
		fprintf(fp, "SELECT DISTINCT %s FROM %s,%s\n", key,  
			table, joinargs[0]);
		fprintf(fp, "WHERE %s \n", where);
		fprintf(fp, "AND %s=%s AND %s IS NOT NULL\n", 
			joinargs[1], joinargs[2], key);
		fprintf(fp, "ORDER BY %s \n", key); 
	}
	else {		   
		fprintf(fp, "SELECT DISTINCT %s FROM %s\n", key, table);
		fprintf(fp, "WHERE %s AND %s IS NOT NULL\n", where, key);
		fprintf(fp, "ORDER BY %s \n", key);
	}

	fclose(fp);


	/* Execute SQL command				*/
        sprintf(sysbuf,"isql %s %s > %s",
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
	while (!feof(fpin) ) {
            if(fscanf(fpin,"%s", buf1 ) )
               	if ((strlen(buf1) >0) && (strncmp(buf1,key,strlen(key)) != 0) ) 
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

