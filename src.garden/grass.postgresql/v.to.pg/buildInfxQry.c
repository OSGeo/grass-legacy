#include "gis.h"
#include "Vect.h"
#include <stdio.h>
#include <string.h>
#include "dbvect.h" 

int buildInfxQry(where, map, mapset, color, fillcolor)
  char *where, *map, *mapset;
  int color, fillcolor;
  {
    int stat;
    static char SQL_stmt [1024];
    static char wherecl[8] = "";

    stat = 1;
           
    if (where)  {
       strcpy (wherecl," where ");

/************************ BEGIN SQL Processing ************************/
    snprintf(SQL_stmt,1024, "SELECT Distinct %s from %s %s %s and %s is not null",key_string,table_string,wherecl,where,key_string);
    } else
    snprintf(SQL_stmt,1024, "SELECT Distinct %s from %s where %s is not null",key_string,table_string,key_string);   
/* distinct has implicit order by */

		
    stat = runInfxFile( SQL_stmt, map, mapset, color, fillcolor );
		
    return (stat);
}

