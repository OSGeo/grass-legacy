/* sqlQry - Do it
 *
 * INGRES modifications - '\g' is added after each SQL query
 *
 *			  THe INGRES output is in a "nice looking" table
 *			  format. The routine ingresUnTable has been added
 *			  to remove all characters that make up the table
 *			  frames etc, as well as any lines of text (except
 *			  column header).
 * 
 *			  The ingresUnTable routine differs slightly for
 *			  INGRES ver5.0 and ver6. The currently implemented
 *			  version applies to INGRES ver 6.
 * 
 *			  The column headers in the output from INGRES SQL
 *			  may be truncated. Strcmp between the output and
 *			  the column name (key) is limited to the length of
 *			  the SQL output.
 *
 *			  The syntax of the execution of the SQL command
 *			  has been modified.
 *
 * Enhancements - Fancy display with title and legend added.
 * 		  SQL queries on the form (select * from ...) are 
 *		  handled. The first column is extracted from the
 *		  output, assuming that this column is the key column.
 *
 * KJ 930415
 */

#include "gis.h"
#include "ingresUtils.h"
#include "displayUtils.h"
#define LINE 80
#define TRUE 0
#define FALSE 1

sqlQry(selectFile,key,raster,rastout,label)
  char *selectFile, *key, *raster, *rastout, *label;
  {
    FILE *fpin, *fpout;
    int catcnt, hit, TMP;
    char ch;
    char buf[1024];
    char *tmpfile_out ;
    char *tmpfile_sql;
    char *tmpfile_rules ;
    char *reduced_key;

/* Not needed for INGRES. KJ 930415  
    if ( (reduced_key = rindex(key,'.')) != NULL )
       key = reduced_key + 1;
*/

    catcnt = 0;
    TMP = FALSE;

           
/* Check for presence of selectfile first */
	if((fpin = fopen(selectFile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql).\n");
	    exit(-1);
           }

/****** Added for INGRES, KJ930415 *******/

	printf("Reading SQL input file ...\n");
	
	/* Add \g after each query Kj 930415 */
	tmpfile_sql = G_tempfile();
	if((fpout = fopen(tmpfile_sql,"w")) == NULL) {
		fprintf(stderr, "File write error on temporary file (tmp_sql).\n");
		exit(-1);
	}
	ch = getc(fpin);
	while (!feof(fpin)) {
		fprintf(fpout, "%c",ch);
		if (ch==';') fprintf(fpout,"\\g\n");
		ch = getc(fpin);
	}

	fclose(fpin);
	fclose(fpout);

/* Check that the file is OK */
	if((fpin = fopen(tmpfile_sql, "r")) == NULL) {
		fprintf(stderr, "File read error on temporary file (tmp_sql). \n");
		exit(-1);
	}
/****** End INGRES addition **************/

	tmpfile_out = G_tempfile();

	printf("Querying database ...\n");
        
	/* SQL syntax modified for INGRES. KJ930415 */
	sprintf(buf,"sql %s < %s > %s",
		G_getenv("DATABASE"),tmpfile_sql,tmpfile_out);
        system(buf);
	
	if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (sql output).\n");
	    exit(-1);
           }

    	tmpfile_rules= G_tempfile() ;
	if((fpout = fopen(tmpfile_rules,"w")) == NULL) {
            fprintf(stderr, "File write error on temprorary file (rules).\n");
	    exit(-1);
           }

/* Modified for INGRES. KJ 930415 */
	printf("Creating GRASS reclass rules ...\n");
        while (!feof(fpin) )
		if(fgets(buf,LINE,fpin)) {
		   ingresUnTable(buf);
		   G_squeeze(buf);
		   ingresGetKey(buf); 
		   if ( (strlen(buf) > 0) && (strncmp(buf,key,strlen(buf)) != 0) ) { 
                        /*G_squeeze(buf);*/
			if (label)
             		   fprintf(fpout,"%s=%d %s(%d)\n",buf, catcnt, label,catcnt);
			else {
			   fprintf(fpout,"%s=%d\n",buf, catcnt);
			   } 
		   }	
		   else   { /* increment cat count - if buf=key then new Q results */
				if((strlen(buf) >0) && (strncmp(buf,key,strlen(buf)) == 0))
		    			catcnt++;
		   }
		}

        fprintf(fpout,"end\n");
        fclose(fpin);
        fclose(fpout);


	if (!rastout) {
	    rastout="tmp.recl" ;
	    TMP = TRUE;
	}

	printf("Reclassifying data ...\n");
	sprintf(buf,"r.reclass input=%s output=%s < %s\n",
		raster, rastout, tmpfile_rules);

	system(buf);
	  
	 printf("Displaying result ...\n");	 
/*
	sprintf(buf,"d.rast %s\n",rastout);
	system(buf);
*/
	fancyDisplay(rastout);

	unlink(tmpfile_out);
    	unlink(tmpfile_sql);
	unlink(tmpfile_rules);

	/* if user didn't specify raster output file - remove */
	if (TMP==TRUE) {
		G_remove("cell","tmp.recl") ;
		G_remove("cats","tmp.recl") ;
		G_remove("cellhd","tmp.recl") ;
		G_remove("hist","tmp.recl") ;
		G_remove("cell_misc","tmp.recl") ;
		G_remove("colr","tmp.recl") ;
	}
	return 0 ;
		
}

