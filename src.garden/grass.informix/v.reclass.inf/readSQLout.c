#include "gis.h"
#define TRUE 0
#define FALSE 1

int readSQLout(tmpfile_out, outFlag, key, col, input, output, vtype, disolve)

	char *tmpfile_out;
	int outFlag, disolve;
	char *key, *col, *input, *output, *vtype;
{

    FILE *fp, *fpin, *fpout;
    int i,  cat, TMP;
    char sysbuf[1024];          /* buffer to issue system cmd   */
    char buf[1024];             /* value in key column          */
    char label[1024];           /* current value in column      */
    char prev_label[1024] ;             /* running value for column     */
    int  colFlag, x;
    char *tmpfile_rules;
    char *reduced_col, *reduced_key;
    char *rindex();

    if ( (reduced_col = rindex(col,'.')) != NULL )
       col = reduced_col + 1;

    if ( (reduced_key = rindex(key,'.')) != NULL ) 
       key = reduced_key + 1;

    i = 1;
    colFlag =0;
    cat = 1;
    TMP = FALSE;

        /* Open file containing output from SQL command */
        if((fpin = fopen(tmpfile_out,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (output)\n");
            exit(-1);
           }


        /* Open file for reclass rules 			*/
        tmpfile_rules = G_tempfile() ;
	if((fpout = fopen(tmpfile_rules,"w")) == NULL) { 
            fprintf(stderr, "File write error on temporary file (rules)\n");
	    exit(-1);
           }



	/* Read SQL output and generate reclass rules	*/

	   *prev_label = 0;
	   while(G_getl(buf, sizeof(buf), fpin) ) {
		G_squeeze(buf);
		if (*buf == 0) continue;
		if ((outFlag == 1) && (colFlag == 0)) { colFlag = 1; continue; } /* skip 1st line in col output */

                if (outFlag == 1 && *buf > 0)           /* Read column type output */
			if(strncmp(buf,key,strlen(key)) == 0 ) { cat++; continue; }
                        if(sscanf(buf,"%d %[^\n]",&x, label) != 2) {
                                fprintf(stderr,"Col error %s\n", buf); exit(-1);
                          }
                         else {
				/* Original 4.1 code treats all rows as individual cats but won't do multiple Q's
                                G_squeeze(label);
                                if (strcmp(label,prev_label) != 0) cat++;
                                fprintf(fpout,"%d=%d %s\n", x, cat, label);
                                strcpy(prev_label, label);
				*/
				if (disolve)	/* then don't print labels, they make no sense */
                                	fprintf(fpout,"%d=%d \n", x, cat);
				  else {
                                	G_squeeze(label);
                               		fprintf(fpout,"%d=%d %s\n", x, cat, label);
                                	strcpy(prev_label, label);
				}
                        }

                if (outFlag == 2 && *buf > 0)           /* Read row type output */
                        if (strncmp(buf,key,strlen(key)) == 0)  {         /* Read: Key Keyval     */
                                if(sscanf(buf,"%*s %d", &x) != 1) {
                                        fprintf(stderr,"Row error %s\n", buf); exit(-1);
                                }
			}
			 
			if (strncmp(buf,col,strlen(col)) == 0)
                                if(sscanf(buf,"%*s %[^\n]", label) != 1) { 	/* Read: Col Colval     */
                                        fprintf(stderr,"error %s\n", buf); exit(-1);
                                }
                                else {
                                        G_squeeze(label);
                                        if (strcmp(label,prev_label) != 0) cat++;
                                        fprintf(fpout,"%d=%d %s\n", x, cat, label);
                                        strcpy(prev_label, label);
                                }

                          }
	
        fclose(fpin);
        fclose(fpout);

	/* If user didn't specify output file		*/
	if (!output) {
	    output="tmp.recl" ;
	    TMP = TRUE;
	}


	/* Execute GRASS reclass			*/
	if (disolve)
		sprintf(sysbuf,"v.reclass input=%s output=%s type=%s -d < %s\n",
		input, output, vtype, tmpfile_rules);
	  else 
		sprintf(sysbuf,"v.reclass input=%s output=%s type=%s < %s\n",
		input, output, vtype, tmpfile_rules);
	system(sysbuf);
	   

	/* Remove temporay dot files			*/
	unlink(tmpfile_out);
	unlink(tmpfile_rules);

return 0;
}

