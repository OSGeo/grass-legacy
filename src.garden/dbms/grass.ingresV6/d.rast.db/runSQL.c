/*
 * runSQL.c - Execute the SQL query, edit the answer and convert to 
 *            GRASS reclass rules.
 *
 * INGRES modifications - INGRES requires the query to end with \g (GO)
 *			  to invoke execution.
 * 			  \g is added to all queries (whether generated
 * 			  by the program or by the user).
 *			  
 *  			  The syntax of the SQL command has been modified
 *			  for INGRES.
 *			
 *			  The modifications work with INGRES ver5.0 and ver6.
 *
 * KJ 930411
 */
			  
#include "gis.h"
#include "dbrast.h"

extern char dbkey[1024];
extern char dbcol[1024];

runSQL(sqlin, input, output, key, col, ALLOPTS )
  char *sqlin,*input, *output, *key, *col;
  int ALLOPTS ;
  {
    FILE *fp, *fpin, *fpout;
    char sysbuf[1024];		/* buffer to issue system cmd 	*/
    int outFlag, readFlag ;
    char *tmpfile_out, *tmpfile_in;
    char ch;

    tmpfile_out = G_tempfile();
    tmpfile_in = G_tempfile();
    outFlag = readFlag =0;
  
          
/****** Added for INGRES (KJ 930411) ****/ 

        /* Open file for input SQL commands */
        if((fpin = fopen(sqlin,"r")) == NULL) {
            fprintf(stderr, "File read error on select file (%s)\n",sqlin);
            exit(-1);
           }

	/* Open file for output SQL commands */
	if((fpout = fopen(tmpfile_in,"w")) == NULL) {
		fprintf(stderr, "File write error on temporary file \n");
		exit(-1);
	}

	ch = getc(fpin);
	while (!feof(fpin)) {
		fprintf(fpout, "%c",ch);
		ch = getc(fpin);
	}
	fprintf(fpout,"\\g\n");

	fclose(fpin);
	fclose(fpout);

/***** End INGRES addition **************/

	/* Open temporary file for read */

        if((fpin = fopen(tmpfile_in,"r")) == NULL) {
            fprintf(stderr, "File read error on select file (%s)\n",tmpfile_in);
            exit(-1);
	}
	

/*************** BEGIN SQL Processing (modified for INGRES)***************/

	/* Execute SQL command				*/

	printf("Querying the database ... \n");
        sprintf(sysbuf,"sql -s %s < %s > %s",
		G__getenv("DATABASE"),tmpfile_in,tmpfile_out);
        system(sysbuf);


	/* Check SQL output type row or column and -s flag options	*/
	if ((outFlag = row_or_col(tmpfile_out, ALLOPTS)) < 1)
		{ fprintf(stderr, "Row/Column error on SQL output.\n"); exit(-1); }

	if (ALLOPTS == 0) { key=dbkey; col=dbcol; } 
	if (readFlag = (readSQLout(tmpfile_out, outFlag, key, col, input, output)) != 0)
		{ fprintf(stderr, "Read error on SQL output.\n"); exit(-1); }

	return(readFlag) ;
}	
