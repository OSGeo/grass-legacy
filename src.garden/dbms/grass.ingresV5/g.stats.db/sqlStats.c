/* sqlStats -
 *
 * INGRES modifications - Name of header files changed:
 *			  ingres.h <=> infx.h
 *			  ingstats.h <=> stats.h
 *			  Stuff in header files never used.
 *
 *			  Reading output SQL query modified. Routine
 * 			  ingresUnTable added.
 *
 * Katarina Johnsson 930419
 */

#include "gis.h"
#include "ingstats.h"
#include "ingres.h"
#include "ingresUtils.h"
#define LEN 20
#define LINE 80
#define HEADER "coltype"
#define PAGE 24

sqlStats(tabname, colname,sqlFile)
  char *tabname;
  char *colname;
  char *sqlFile;
  {
    FILE *fp;
    int i, hit;
    char buf[1024];
    char *statsFile;
    int colFlag;

    /* Added for INGRES, KJ 930419 */
    int count, sum;
    float mean, max, min;

    i = 0;

    /* Check that input file is OK, Open file for read NOT write, KJ 930419 */	

    if((fp = fopen(sqlFile,"r")) == NULL) {
		fprintf(stderr, "File read error on temporary file\n");
			    exit(-1);
	}


    /*  File is already built in main, KJ 930419 	
    buildSQL(fp, tabname, colname); 
    */
    fclose(fp) ;

    statsFile = G_tempfile() ;

    /* Syntax modified for INGRES, KJ 930419 */
    printf("Getting statistics from database ...\n");
    sprintf(buf,"sql %s < %s > %s",
		G__getenv("DATABASE"),sqlFile,statsFile);
    system(buf);
    if((fp = fopen(statsFile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }

	printf("\nStatistics for column: %s\n\n",colname);

	/* Reading stats, modified for INGRES, KJ 930419 */

	colFlag = 0;
	while(G_getl(buf, sizeof(buf), fp) ) {
		ingresUnTable(buf);	
		G_squeeze(buf) ; 
		if (*buf == 0) continue;
		if (colFlag == 0) {
			printf("|COUNT     |SUM       |MEAN      |MAX       ");
			printf("|MIN       |\n");
			printf("|");
			for (i=0;i<54;i++) printf("-");
 			printf("|\n");
			colFlag=1;
			continue;
		}
		if(sscanf(buf,"%d %d %f %f %f", &count, &sum, &mean, &max, 
			  &min) == 5) {
			printf("|%10d|%10d|%10f|%10f|%10f|\n", 
				count, sum, mean, max, min);
		}
	}

	printf("|");
	for (i=0;i<54;i++) printf("-");
        printf("|\n");
	printf("(1 rows)\n\n");

        fclose(fp);
	unlink(statsFile); 

	return 0;

		
}

