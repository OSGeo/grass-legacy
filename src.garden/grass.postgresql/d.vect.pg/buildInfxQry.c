/* d.vect.pg  */
/* buildInfxFile.c    */
/* modifications 11/1998 Carl Anderson */
/* mods Alex Shevlakov Jan.00*/


#include "gis.h"
#include "Vect.h"
#define TRUE 0
#define FALSE 1

buildInfxQry(key, where, table, map, mapset, color, fillcolor)
  char *key,*where, *table, *map, *mapset;
  int color, fillcolor;
  {
    int stat;
    static char SQL_stmt [1024];
    static char wherecl[8] = "";

    stat = 1;
           
    if (where)  {
       strcpy (wherecl," where ");

/************************ BEGIN SQL Processing ************************/
    snprintf(SQL_stmt,1024, "SELECT Distinct %s from %s %s %s and %s is not null",key,table,wherecl,where,key);
    } else
    snprintf(SQL_stmt,1024, "SELECT Distinct %s from %s where %s is not null",key,table,key);   
/* distinct has implicit order by */

		
    stat = runInfxFile( SQL_stmt, map, mapset, color, fillcolor );
		
    return (stat);
}

