/* d.site.pg   */
/* buildInfxQry.c  */
/*-----------  A.Sh. */

#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include "dbsite.h"

#define TRUE 0
#define FALSE 1

buildInfxQry(table, x, y, cat, where, map, joinargs, plotargs )
  char *table;
  char *x;
  char *y;
  char *cat;
  char *where;
  char *map;
  char *joinargs;
  char *plotargs[];
  {
    static char SQL_stmt[1024];
    char  wherecl[1024] = "";
    char  catcl[80] = "";
    int i;

    if ( where ) 
       { snprintf (wherecl,1024, "%s and",where); }
/* build a clause to hold where clause -- if it exists */

    if ( cat )
       { snprintf (catcl,80, ", %s ",cat); }
/* build a cluase to hold category support -- if it exists */
/************************ BEGIN SQL Processing ************************/

snprintf(SQL_stmt,1024, "SELECT %s,%s %s from %s where %s %s is not null and %s is not null order by %s,%s",x,y,catcl,table,wherecl,x,y,x,y);		

/* Now that the SQL has output COORDS plot points 	*/

  i = runInfxFile( SQL_stmt, map, plotargs );
  return(i) ; 	 
}

