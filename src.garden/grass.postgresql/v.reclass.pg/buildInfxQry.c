/* v.reclass.pg   */
/* buildInfxQry.c  */

/* A.Sh - 12.01.00
*/

#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#define TRUE 0
#define FALSE 1

buildInfxQry(key,col,table,where,input,output, vtype, disolve)
  char *key;
  char *col;
  char *table;
  char *where;
  char *input;
  char *output;
  char *vtype;
  int disolve;
  {
    static char SQL_stmt[1024];
    char  wherecl[1024] = "";

    int i;

    if ( where ) snprintf (wherecl,1024, "%s and",where);
       
/* build a clause to hold where clause -- if it exists */


/************************ BEGIN SQL Processing ************************/

snprintf(SQL_stmt,1024,
	"SELECT %s,%s from %s where %s %s is not null and %s is not null order by %s,%s",key,col,table,wherecl,key,col,key,col);		



  i = runInfxFile( SQL_stmt,input,output, vtype, disolve);
  return(i) ; 	 
}

