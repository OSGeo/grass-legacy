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
                if (*buf == 0) continue;

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

