#include "gis.h"
#include "Vect.h"
#include <stdio.h>
#include "dbvect.h"
#define TRUE 0
#define FALSE 1
#define         DIG_DIR         "dig"
#define         ATT_DIR         "dig_att"


infxQry(selectFile, input, output, mapset )
  char *selectFile, *input, *output, *mapset;
  {
    FILE *fp, *fpin, *fpout;
    int i;
    int line_cat;		/* vector cat values from db 	*/ 
    int catcnt;			/* current value for reclassed V's */
    int curcat;			/* current cat value 		*/
    int curLine;
    int day, yr, vect_read;
    char label[200];
    char sysbuf[1024];		/* buffer to issue system cmd 	*/
    char buf1[1024];		/* value in key column		*/
    char name[1024];
    char mon[4], date[40] ;
    struct line_pnts *Points ;
    struct Map_info Map;
    struct Map_info New_Map;
    struct Categories newcats;
    char *tmpfile_out ;




	curLine = 1;
	tmpfile_out = G_tempfile() ;

	/* Check for selectFile first */
        if((fpin = fopen(selectFile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql).\n");
            exit(-1);
           }

        sprintf(sysbuf,"sqlplus -s %s @%s | awk '{ if (NF == 1) { print $1 }  else { print $0 }  }' >%s",
                G_getenv("DATABASE"),selectFile,tmpfile_out);

        system(sysbuf);

        if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql output).\n");
            exit(-1);
           }

	/* Now that the SQL has output CAT value(s) do reclass */

	/* Initialize new Vector stuff */

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
    	G_set_cats_title("Prod. by v.reclass.ora",&newcats);

	/* Read and write header info */
        sprintf(date,"%s",G_date());
        sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
        if (yr < 2000) yr = yr - 1900;
        else yr = yr - 2000;
        sprintf(date,"%s %d %d",mon,day,yr);
        Vect_copy_head_data(&Map.head, &New_Map.head);
        strcpy( New_Map.head.date,date);
        strcpy( New_Map.head.your_name,output);
        strcpy( New_Map.head.map_name,"Created by v.reclass.ora");

	build_lookup_tables (&Map) ;

	line_cat = 0;
	catcnt = 0;
	curcat = -1;

	/* Read SQL output and reclass vectors */
	while (!feof(fpin) ) {
            if(fscanf(fpin,"%s", buf1 ) )
               if (strlen(G_squeeze(buf1)) >0)  {
			if( catcnt != curcat )  {	 /* update cats file */
                    		fprintf (stdout,"%s category %d",
				G__getenv("DATABASE"),line_cat);
                    		sprintf(label,"%s category %d",
				G__getenv("DATABASE"),line_cat);
                    		G_set_cat(line_cat,label,&newcats);
                    		G_write_vector_cats(output,&newcats);
				curcat++ ;
			}
			if(line_cat != atoi(buf1) ) {
			   line_cat = atoi(buf1);
			   curLine=modCat(output, &New_Map, &Map, Points, line_cat, catcnt,fpout);
			}
		  }
		else
		    catcnt++;

	}
   

	fclose(fpout);
        fclose(fpin);
	unlink(tmpfile_out) ;
	G_free_cats(&newcats);
	Vect_close(&Map) ;
	Vect_close(&New_Map) ;
	return 0;
		
}

