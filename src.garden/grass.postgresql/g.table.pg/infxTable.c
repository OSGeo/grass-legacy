#include "gis.h"
#include <stdlib.h>
#include <libpq-fe.h>


	/*A.Sh. 12.99 -trimmed for pgsql 6.5*/

int infxTables()
  {
   
    int i;

    PGconn     *pg_conn;
    PGresult   *res;
    char       *pghost;
    char	*pgdbase;
    int	       rec_num;

pghost = G__getenv ("PG_HOST");
pgdbase = G_getenv ("PG_DBASE");
pg_conn = PQsetdb(pghost,NULL,NULL,NULL,pgdbase);

if (PQstatus (pg_conn) == CONNECTION_BAD) {
    fprintf (stderr, "Error: select Postgres:%s\n",
       PQerrorMessage(pg_conn));
    exit (-1);
  }
  
/* res  = PQexec(pg_conn, "select tablename from pg_tables where tableowner <>
'postgres' union select viewname from pg_views where viewowner <>
'postgres'");*/
 
res  = PQexec(pg_conn, "select tablename from pg_tables where tablename !~ 'pg_*' order by tablename");
 if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
    printf ("Error:connect Postgres:%s\n",PQerrorMessage(pg_conn));
    PQfinish(pg_conn);
    exit (-1);      
  } 	   
 
rec_num = PQntuples (res);
 for ( i=0; i < rec_num; i++)
       printf ("%s\n", PQgetvalue(res,i,0));
 
    
PQclear (res);
PQfinish (pg_conn);    

exit(0);
}
