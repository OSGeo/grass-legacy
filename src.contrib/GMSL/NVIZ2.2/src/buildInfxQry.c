#include "gis.h"
#include <stdio.h>


char* buildInfxQry(ktab,keycat,curcat)
	char *ktab, *keycat; 
	int curcat;
{

	
	static char SQL_stmt[1024] ;

	
	snprintf(SQL_stmt, 1024,
          "SELECT * from %s where %s=%d",ktab,keycat, curcat);
	
	return (SQL_stmt) ;

}

char* buildInfxsite(ktab,ycol, xcol)
	char *ktab, *ycol, *xcol; 
{
	static char SQL_stmt[1024] ;

           snprintf(SQL_stmt,1024, 
          "SELECT * from %s where point(%s,%s) ",ktab, xcol, ycol);

	return (SQL_stmt) ;

}


