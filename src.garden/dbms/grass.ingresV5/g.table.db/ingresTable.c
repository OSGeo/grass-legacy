/* MODIFIED by Noel Krahn
 * DATE:Fri Oct 30 15:09:50 PST 1992
 */

#include "gis.h"
#define TABLEN 20

ingresTable(sqlFile)
  char *sqlFile;
  {
    FILE *fp;
    int buflen, i;
    char buf[1024];
    char *tmpfile;

    printf("The following tables are available in database: %s\n",
      G__getenv("DATABASE") );
    printf("(Querying database ...) \n");
    
    sprintf(buf,"sql -s %s < %s", G__getenv("DATABASE"), sqlFile);
    system(buf);
}

