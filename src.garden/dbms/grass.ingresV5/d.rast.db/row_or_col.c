/* row_or_col.c - Reads output from SQL query. If the query was user
 * 		  generated, the column names (key and col) are extracted
 * 		  from the SQL output.
 *
 * Comment: 	  The main purpose of this routine seems to be to extract
 * 		  the column names (in the case of user generated queries)
 * 		  The routine reads the entire SQL output file, but doesn't
 * 		  save the result in a new form (more suited for the GRASS
 * 		  reclass rules. It seems like a waste to read the whole
 * 		  file, but for now no improvements have been made.
 * 
 * INGRES modifications - The INGRES output is a "nice looking" table, with
 *			  frames. The routine ingresUnTable has been added
 * 			  to remove all characters that make up the frames
 *			  as well as any lines of text (except the table
 * 			  header).
 *
 *			  ingresUnTable is different for INGRES ver5.0 and
 *			  ver6.
 *
 *			  The row type output does not apply to INGRES.
 * 
 * KJ 930411
 */

#include "gis.h"
char dbkey[1024];
char dbcol[1024];

int row_or_col(tmpfile_out, ALLOPTS) 

	char *tmpfile_out ;
	int ALLOPTS;
{
	FILE *fpin ;
	int val;
	int stat = 0;
	char buf[1024], v1[1024], v2[1024] ;


        /* Open file containing output from SQL command */
        if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (output)\n");
            exit(-1);
           }

	while (G_getl(buf, sizeof(buf), fpin) && (stat == 0)) {
                G_squeeze(buf);
		ingresUnTable(buf);
                if (*buf == 0) continue;

/* Row type output doesn't apply to INGRES, KJ 930411 */

		if (sscanf(buf,"%s %d", v1, &val) == 2)    {	/* Row type output reads name and val */
			if (ALLOPTS == 0)	{		/* -s command line flag, get key,col  */
				G_getl(buf, sizeof(buf), fpin);
				G_squeeze(buf);
				if (sscanf(buf,"%s", v2) == 1) { strcat(dbkey,v1); strcat(dbcol,v2); stat = 2; }
				fclose(fpin) ;
				return stat ;
			}
			else { stat = 2; }
		}
         	else {
                 	if (sscanf(buf,"%s %s", v1, v2) == 2)  { /* Col type output reads header */
				if (ALLOPTS == 0) {		 /* -s command line flag, get key, col */

/* Note that the column headers may be truncated in INGRES. The width of
   INGRES table columns depends on the data type. KJ 930411             */

					strcat(dbkey,v1) ;
					strcat(dbcol,v2) ;
                     			stat = 1;
			     }
			 else { stat = 1; }
			}
         	}
	}
	fclose(fpin) ;
	return stat ;
}

