/* chktype - 
 *
 *
 * INGRES modifications - Name of header files changed:
 *			  ingres.h <=> infx.h
 *			  ingstats.h <=> stats.h
 * 			  Stuff in ingstats.h never used.
 *
 *			  Query on datatype applies to INGRES ver5.0!!
 *			  It has to be modified for INGRES ver6.
 *
 *			  Subroutine moved to main:
 *			  sqlStats <=> infxStats
 *
 *			  Argument 'sqlFile' is NOT passed to this routine.
 *			  A new temporary file is created for query to
 *			  database about column type.
 *
 *			  Syntax for execution of SQL modified.
 *
 *			  Reading output from SQL query modified. Routine
 *			  ingresUnTable added.
 *
 *			  Test for numeric datatype modified for INGRES ver5.0:
 *				integer 30
 *				float 31
 *			  This will not work with INGRES ver6!!
 *
 * Katarina Johnsson 930419
 */

#include "gis.h"
#include "ingstats.h"
#include "ingres.h"
#include "ingresUtils.h"
#define LINE 80
#define HEADER "coltype"

chktype(tabname, colname)
  char *tabname;
  char *colname;
  /* char *sqlFile ; */
  {
    FILE *fp;
    char buf[1024];
    char *tmpfile;
    char *sqlFile;
    int stat;
    int colFlag;

	stat = 0;

	printf("Checking column datatype ... \n");
           
	sqlFile = G_tempfile() ;
	
	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

	tmpfile = G_tempfile();


	/* Query parsed here instead of using CHKTYP in header file, KJ */
        fprintf(fp,"SELECT attfrmt FROM attribute\n");
	fprintf(fp,"WHERE attname='%s'\n",colname);
	fprintf(fp,"AND attrelid='%s'\n", tabname);
	fprintf(fp, "\\g\n");
	fclose(fp);

	/* Modified for INGRES, KJ 930419 */
        sprintf(buf,"sql %s < %s > %s",
		G__getenv("DATABASE"),sqlFile,tmpfile);

	system(buf);
	unlink(sqlFile);

	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }

	/* Modified for INGRES KJ 930419 */
        /* integer = 30, float=31 in SQL output. Valid for INGRES ver5.0 only */
	colFlag = 0;

	while(G_getl(buf, sizeof(buf), fp)) {
		ingresUnTable(buf);
		G_squeeze(buf);
		if(*buf ==0) continue;
		printf("BUF: %s\n", buf);
		if (colFlag ==0) { colFlag=1; continue; }
	   	if ( ! ((atoi(buf) > 0) && ((atoi(buf) == 30) || (atoi(buf) == 31)))) {
 			   fprintf(stderr,"Stats: Numeric type only (%s).\n",
				   colname);
			   exit(-1);
	   	}
	}

/* Moved to main, KJ 930419 
        stat = sqlStats(tabname, colname, sqlFile); 
*/
	unlink(tmpfile);

	return 0;
   }
