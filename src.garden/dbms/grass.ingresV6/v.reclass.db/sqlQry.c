/* sqlQry - do it!
 *
 * INGRES modifications - '\g' is added after each SQL query.
 *
 *			  The INGRES output is in a "nice looking" table
 *			  format. The routine ingresUnTable has been added
 *			  to remove all characters that make up the table
 *			  frames etc, as well as any lines of text (except
 * 			  column header).
 *
 *			  The ingresUnTable routine differs slightly for 
 *			  INGRES ver5.0 and ver6. The currently implemented
 *			  version applies to INGRES ver 6.
 *
 * 			  The column headers in the output from INGRES SQL
 * 			  may be truncated. Strcmp between the output and
 *			  the column name (key) is limited to the length of
 *			  the SQL output.
 *		
 *			  The syntas of the execution of the SQL command 
 *			  has been modified.
 *
 * Enhancements - SQL queries on the form (select * from ...) are
 * 		  handled. The first column is extracted from the
 *		  output, assuming that this column is the key column.
 *
 * Bug Fixes - Label changed to 'db_name category catcnt' from 
 *	       'db_name category line_cat' to get consistency with
 *	       r.reclass.db.
 *
 *	       Control of loop to update cats file and modify vectors
 *	       changed to 'firstFlag' from 'curcat' to get correct loop
 *	       control.
 *
 *	       First argument in G_set_cat changed to 'catcnt' from
 *	       'line_cat' to get correct new category number.
 *
 * KJ 930416
 */


#include "gis.h"
#include "Vect.h"
#include "ingresUtils.h"
#include <stdio.h>
#include "dbvect.h"
#define TRUE 0
#define FALSE 1
#define         DIG_DIR         "dig"
#define         ATT_DIR         "dig_att"


sqlQry(selectFile, key, input, output, mapset )
  char *selectFile, *key,*input, *output, *mapset;
  {
    FILE *fp, *fpin, *fpout;
    int i;
    int line_cat;		/* vector cat values from db 	*/ 
    int catcnt;			/* current value for reclassed V's */
    int curcat;			/* current cat value 		*/
    int curLine;
    int day, yr, vect_read;
    char *label;
    char sysbuf[1024];		/* buffer to issue system cmd 	*/
    char buf1[1024];		/* value in key column		*/
    char name[1024];
    char mon[4], date[40] ;
    struct line_pnts *Points ;
    struct Map_info Map;
    struct Map_info New_Map;
    struct Categories newcats;
    char *tmpfile_out ;
    char *tmpfile_sql;
    char ch;
    int firstFlag;




	curLine = 1;

	/* Check for selectFile first */
        if((fpin = fopen(selectFile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql).\n");
            exit(-1);
           }

/***** Added for INGRES, KJ 930416 *********/

	printf("Reading SQL input file ...\n");

	/* Add \g after each query */
	tmpfile_sql = G_tempfile();
	if ((fpout = fopen(tmpfile_sql,"w")) == NULL) {
		fprintf(stderr, "File write error on temporary file (tmp_sql),\n");
		exit(-1);
	}
	ch = getc(fpin);
	while (!feof(fpin)) {
		fprintf(fpout, "%c",ch);
		if (ch==';') {fprintf(fpout,"\\g\n");}
		ch = getc(fpin);
	}

	fclose(fpin);
	fclose(fpout);

/* Check that the file is OK */
	if ((fpin = fopen(tmpfile_sql, "r")) == NULL) {
		fprintf(stderr, "File read error on temporary file (tmp_sql),\n");
		exit(-1);
	}

/***** end INGRES addition ***********/
		
	tmpfile_out = G_tempfile();

	printf("Querying database ..\n");

	/* SQL syntax modified for INGRES, KJ930416 */
        sprintf(sysbuf,"sql %s < %s > %s",
                G_getenv("DATABASE"),tmpfile_sql,tmpfile_out);
        system(sysbuf);


        if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temproary file (sql output).\n");
            exit(-1);
           }

	/* Now that the SQL has output CAT value(s) do reclass */

	/* Initialize new Vector stuff */

	printf("Initializing ...\n");

	if(Vect_open_old(&Map,input,mapset) < 0  ) {
           fprintf(stderr,"Can't open input vector file <%s> \n", input) ;
           return (-1);
           }

        if ( Vect_open_new(&New_Map, output) < 0)
           {
           fprintf(stderr,"Can't create output vector file <%s> \n", output) ;
           return (-1);
           }

        if ( (fpout = G_fopen_new(ATT_DIR, output)) == NULL)
           {
           fprintf(stderr,"Can't create output attribute file <%s> \n", output) ;
           return (-1);
           }


	/* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();

	/* Initialize new category structure */
	G_init_cats((CELL)0,"",&newcats);
    	G_set_cats_title("Produced by v.reclass.db",&newcats);



	/* Read and write header info */
        printf("Getting header info ...\n");

        sprintf(date,"%s",G_date());
        sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
        if (yr < 2000) yr = yr - 1900;
        else yr = yr - 2000;
        sprintf(date,"%s %d %d",mon,day,yr);
        Vect_copy_head_data(&Map.head, &New_Map.head);
        strcpy( New_Map.head.date,date);
        strcpy( New_Map.head.your_name,output);
        strcpy( New_Map.head.map_name,"Created by v.reclass.db");


	build_lookup_tables (&Map) ;

	line_cat = 0;
	catcnt = 0;
	curcat = -1;

/* Modified for INGRES, KJ 930215 */

	firstFlag=1;
	printf("Reclassifying vectors ...\n");
	/* Read SQL output and reclass vectors */
	while (G_getl(buf1,sizeof(buf1), fpin)) {
		ingresUnTable(buf1);
		G_squeeze(buf1);
		ingresGetKey(buf1);
		if (*buf1 == 0) continue;
               if ((strlen(buf1) >0) && 
	 	   (strncmp(buf1,key,strlen(buf1)) != 0) )  {
			if( firstFlag == 0 )  {	 /* update cats file */
                    		sprintf(label,"%s category %d",
				G__getenv("DATABASE"),catcnt);
                    		G_set_cat(catcnt,label,&newcats);
                    		G_write_vector_cats(output,&newcats);
				firstFlag = 1 ;
			}

			if(line_cat != atoi(buf1) ) {
			   line_cat = atoi(buf1);
			   curLine=modCat(output, &New_Map, &Map, Points, line_cat, catcnt,fpout);


			}
		  }
		else /* increment cat count - if buf=key then new Q result */
		   if((strlen(buf1) >0) && 
			(strncmp(buf1,key,strlen(buf1)) ==0)) {
		       		catcnt++;
				firstFlag = 0;
		   }
	}
   

	fclose(fpout);
        fclose(fpin);
	unlink(tmpfile_out) ;
        unlink(tmpfile_sql);
	G_free_cats(&newcats);
	Vect_close(&Map) ;
	Vect_close(&New_Map) ;

	printf("\nDone.\nRun 'd.vect %s' to display new vector map.\n",output);
 
	return 0;
		
}

