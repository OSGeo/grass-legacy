/* d.rast.pg   */
/* buildInfxQry.c  */
/*----------- A.Sh - 13.09.2000 */

#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include "dbrast.h"
#define TRUE 0
#define FALSE 1

buildInfxQry(key,col,lab,table,where,input,output)
  char *key;
  char *col;
  char *lab;
  char *table;
  char *where;
  char *input;
  char *output;
  {
    static char SQL_stmt[1024];
    char  wherecl[1024] = "";
    int withlabel = 0;

    int i;

    if ( where ) 
       { snprintf (wherecl,1024, "%s and",where); }
       
    if ( lab ) 
    	withlabel=1;

if (withlabel) {
snprintf(SQL_stmt,1024, "SELECT %s,%s,%s from %s where %s %s is not null and %s is not null order by %s,%s",key,col,lab,table,wherecl,key,col,key,col);		
} else {
snprintf(SQL_stmt,1024, "SELECT %s,%s from %s where %s %s is not null and %s is not null order by %s,%s",key,col,table,wherecl,key,col,key,col);		
}

  i = runInfxFile( SQL_stmt,input,output,withlabel);
  return(i) ; 	 
}

