#include "gis.h"
#include "stats.h"
#define LINE 80
#define HEADER "coltype"

chktype(tabname, colname, sqlFile)
  char *tabname;
  char *colname;
  char *sqlFile ;
  {
    FILE *fp;
    char buf[1024];
    char *tmpfile;
    int stat;

	stat = 0;
           

        stat = infxStats(tabname, colname, sqlFile); 

	return(stat);
   }
