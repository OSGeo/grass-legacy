#include "gis.h"
#include "stats.h"
#include "infx.h"
#define LEN 20
#define LINE 80
#define HEADER "coltype"
#define PAGE 24

infxStats(tabname, colname,sqlFile)
  char *tabname;
  char *colname;
  char *sqlFile;
  {
    FILE *fp;
    int i, hit;
    char buf[1024];
    char *statsFile;

    i = 0;
    if((fp = fopen(sqlFile,"w")) == NULL) {
		fprintf(stderr, "File write error on temporary file\n");
			    exit(-1);
	}


    buildSQL(fp, tabname, colname);
    fclose(fp) ;

    statsFile = G_tempfile() ;
    sprintf(buf,"isql %s %s > %s",
		G__getenv("DATABASE"),sqlFile,statsFile);
    system(buf);
    if((fp = fopen(statsFile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }

	printf("Statistics for column: %s\n\n",colname);

	while(G_getl(buf, sizeof(buf), fp) ) {
		/* G_squeeze(buf) ; */
		if (*buf == 0) continue;
          	printf("%s\n",buf);
	}

        printf("\n");
        fclose(fp);
	unlink(statsFile); 

	return 0;

		
}

