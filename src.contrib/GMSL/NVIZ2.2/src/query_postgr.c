/*	Alex Shevlakov sixote@yahoo.com 02/2000
*/
#include <stdio.h>
#include "gis.h"
#include "display.h"
#include "Vect.h"
#include "infx.h"



char* query_postgr(name,keytable,col,x,y)

char *name, *keytable, *col;
float x,y;

{


    char *openvect();
    int level;
    static char buf[32]="";
    struct Map_info P_map;
    
    char  *SQL_stmt;
    static char long_str[2048]="";
    char *qry_str;
    
    int dbCat;
    char *mapset, *dbname;
  

	
	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            snprintf(buf,32,
                  "Please run g.select.pg first\n");
	    return buf;
           }


	if ( (mapset = openvect(name) ) == NULL) {
		snprintf(buf,32, "Unable to open %s\n", name);
		return buf;
	}
	

	level = Vect_open_old( &P_map, name, mapset) ;
	if (level < 0)
                G_fatal_error ("Can't open vector file");
        if (level < 2)
                G_fatal_error ("You must first run v.support on vector file");

	
		
        	qry_str=(char*) getCat(&P_map, x,y,&dbCat);
		snprintf(long_str,2048,"%s",qry_str);
		if (dbCat > 0){ 
		SQL_stmt = (char *) buildInfxQry(keytable, col,dbCat);
		qry_str = (char*) runInfxQry(SQL_stmt);
			if (qry_str != NULL) strncat(long_str,qry_str,1024);
			else {
			qry_str="\nThere's been ERROR from Postgres\n";
			strncat(long_str,qry_str,1024);
			}
		}
	
	Vect_close(&P_map);
	return long_str;

}
char* query_pg_site(name,xcol,ycol,dist,x,y)

char *name, *xcol, *ycol;
int dist;
float x,y;

{

char *dbname;
static char buf[32]="";
char *SQL_stmt;
static char long_str[2048]="";
char *qry_str;
struct Sql *pts;
int ret;

/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            snprintf(buf,32,
                  "Please run g.select.pg first\n");
	    return buf;
        }
SQL_stmt = (char*) buildInfxsite(name,ycol,xcol);

	/* Initialze SQL query structure        */
	pts = (struct Sql *)G_malloc(sizeof(struct Sql));
        G_zero (pts, sizeof(struct Sql)); 
	ret = fillSQLstruct(pts,x,y,dist);
	
	long_str[0]='\0';
	
	qry_str = (char*) runqry(SQL_stmt, pts);
	
	if (qry_str != NULL) strncat(long_str,qry_str,1024);
	else {
			qry_str="\nThere's been ERROR from Postgres\n";
			strncat(long_str,qry_str,1024);
	}
return long_str;
}
