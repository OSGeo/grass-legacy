/*	Alex Shevlakov sixote@yahoo.com 02/2000
*/
#include "gis.h"
#include "infx.h"
#include <libpq-fe.h>

char* runInfxQry(SQL_stmt)
	char *SQL_stmt;
	
{
	char buf[1024];
	char chunk[1024]="";
	static char long_str[2048];

	char sqlcmd[1024] ;
	int i,j,nrows,nfields;

    	PGconn *pg_conn;
   	PGresult *res;
    	char    *pghost;
	int vrbs=1;

	
	snprintf(sqlcmd,1024, 
          "%s",SQL_stmt);

 /*if(!strncmp(print_out,"v",1)) vrbs=1;*/
 
 	if(vrbs)  
    	fprintf (stderr,"\n\nExecuting\n%s\n---------------------\n",sqlcmd);
    
    pghost = G__getenv("PG_HOST");      
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));

    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      fprintf (stderr,"Error: select Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      return NULL; 
    }
 
    res = PQexec (pg_conn, sqlcmd);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      fprintf (stderr,"Error: Connecting to Postgres:%s\n",PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      return NULL;      
    }

    nfields = PQnfields(res);
    nrows = PQntuples(res);
    
    long_str[0]='\0';  

	if (nrows == 1 && vrbs) {
		for ( j=0; j < nfields; j++) {
			strncpy (buf, PQgetvalue (res, 0, j),1024);
			snprintf(chunk,1024,"%10s I %s\n",PQfname(res,j),buf);
			strncat(long_str,chunk,1024);	      
		}
	} else if (nrows){
		
			fprintf(stderr,"%s",PQfname(res,0));      
    			for (j=1; j < nfields; j++ ) {
      				fprintf(stderr,",%s",PQfname(res,j));
    			}
    			fprintf (stderr,"\n");
	
    		for ( i=0; i < nrows; i++)  {
      			for ( j=0; j < nfields; j++) {
      				strncpy (buf, PQgetvalue (res, i, j),1024);
   	 			fprintf (stderr,"%s,",buf);  
      			}
      			fprintf (stderr,"\n");
		}
		fprintf(stderr,"\n%d rows selected\n",nrows);
    	} 
	
	if(vrbs){ 
    		snprintf(chunk,1024,"\n%d rows selected\n",nrows);
    		strncat(long_str,chunk,1024);
    	}

    	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

	return long_str ;
}

char* runqry(SQL_stmt, pts)
	char *SQL_stmt;
        struct Sql *pts;
{
	char buf[1024];
	char chunk[1024]="";
	static char long_str[2048];

	char sqlcmd[1024]="";
	int i,j,nrows,nfields;

    	PGconn *pg_conn;
   	PGresult *res;
    	char    *pghost;

	snprintf(sqlcmd,1024, 
          "%s @ '(%f,%f,%f,%f)'::box",SQL_stmt,
            pts->minX,pts->minY,pts->maxX,pts->maxY);
	/* use Postgres graphic operators 
	     @ operator test point in box 
	  cfa 11/98   */
    
    fprintf (stderr,"\n\nExecuting\n%s;\n clause  @ '( )'::box addded autonmatically.\n\n",sqlcmd);
    pghost = G__getenv("PG_HOST");      
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));

    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      fprintf (stderr,"Error: select Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      return NULL; 
    }
 
    res = PQexec (pg_conn, sqlcmd);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      fprintf (stderr,"Error: Connecting to Postgres:%s\n",PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      return NULL;      
    }

    nfields = PQnfields(res);
    nrows = PQntuples(res);
    
    long_str[0]='\0';  

	if (nrows == 1) {
		for ( j=0; j < nfields; j++) {
			strncpy (buf, PQgetvalue (res, 0, j),1024);
			snprintf(chunk,1024,"%10s I %s\n",PQfname(res,j),buf);
			strncat(long_str,chunk,1024);	      
		}
	} else if (nrows){
		
			fprintf(stderr,"%s",PQfname(res,0));      
    			for (j=1; j < nfields; j++ ) {
      				fprintf(stderr,",%s",PQfname(res,j));
    			}
    			fprintf (stderr,"\n");
	
    		for ( i=0; i < nrows; i++)  {
      			for ( j=0; j < nfields; j++) {
      				strncpy (buf, PQgetvalue (res, i, j),1024);
   	 			fprintf (stderr,"%s,",buf);  
      			}
      			fprintf (stderr,"\n");
		}
		fprintf(stderr,"\n%d rows selected\n",nrows);
    	} 
	
 
    		snprintf(chunk,1024,"\n%d rows selected\n",nrows);
    		strncat(long_str,chunk,1024);

    	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

	return long_str ;
}
