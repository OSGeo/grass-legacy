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

    i = 0;
    if((fp = fopen(sqlFile,"w")) == NULL) {
		fprintf(stderr, "File write error on temporary file\n");
			    exit(-1);
	}


    buildSQL(fp, tabname, colname);
    fclose(fp) ;

    fprintf (stdout,"Statistics for column: %s\n\n",colname);
    sprintf(buf,"sqlplus -s %s @%s",
		G__getenv("DATABASE"),sqlFile);
    system(buf);
    fprintf (stdout,"\n");

    return 0;

		
}

