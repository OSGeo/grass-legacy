#include "gis.h"
#include <libpq-fe.h>

runInfxQry(SQL_stmt,print_out)
	char *SQL_stmt;
	char *print_out;
	
{
	char buf[1024]="";

	char sqlcmd[1024]="" ;
	int i,j,nrows,nfields;

    	PGconn *pg_conn;
   	PGresult *res;
    	char    *pghost;
	int vrbs=0;
	
	snprintf(sqlcmd,1024, 
          "%s",SQL_stmt);

  if(!strncmp(print_out,"v",1)) vrbs=1;
 	if(vrbs)      
    fprintf (stderr,"\n\nExecuting\n%s\n---------------------\n",sqlcmd);
    
    pghost = G__getenv("PG_HOST");      
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));

    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      printf ("Error: select Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
 
    res = PQexec (pg_conn, sqlcmd);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf ("Error: Connecting to Postgres:%s\n",PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }

    nfields = PQnfields(res);
    nrows = PQntuples(res);  

	if (nrows == 1 && vrbs) {
		for ( j=0; j < nfields; j++) {
			strncpy (buf, PQgetvalue (res, 0, j),1024);
			fprintf(stderr,"%10s I %s\n",PQfname(res,j),buf);	      
		}
	} else if (nrows){
			fprintf(stderr,"%s",PQfname(res,0));      
    			for (j=1; j < nfields; j++ ) {
      				fprintf(stderr,",%s",PQfname(res,j));
    			}
    			fprintf (stderr,"\n");
		}
    		for ( i=0; i < nrows; i++)  {
      			for ( j=0; j < nfields; j++) {
      				strncpy (buf, PQgetvalue (res, i, j),1024);
   	 			fprintf (stderr,"%s,",buf);  
      			}
      			fprintf (stderr,"\n");
    	} 
	
 	if(vrbs)
    	fprintf(stderr,"\n%d rows selected\n\n",nrows);
    

    	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

	return 0 ;
}
