#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "dbvect.h"

int buildInfxQry(key,col,table,where,input,output, vtype, disolve)
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

snprintf(SQL_stmt,1024,
	"SELECT %s,%s from %s where %s %s is not null and %s is not null order by %s,%s",key,col,table,wherecl,key,col,key,col);		



  i = runInfxFile( SQL_stmt,input,output, vtype, disolve);
  return(i) ; 	 
}

