#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <libpq-fe.h>
#define EXT ".dbs"
#define FS '.'

void listdb(pghost)
char* pghost;
{

PGconn*		pg_conn;
PGresult*	res;
int		num,i;
char* ok;
pg_conn = PQsetdb(pghost,NULL,NULL,NULL,"template1");

 if (PQstatus (pg_conn) == CONNECTION_BAD) {
    printf ("Error: select Postgres:%s\n",PQerrorMessage(pg_conn));
    PQfinish(pg_conn);
    exit (-1); 
  }
  	   
 res  = PQexec (pg_conn, "select datname from pg_database");
 if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
    printf ("Error: connect Postgres:%s\n",PQerrorMessage(pg_conn));
    PQfinish(pg_conn);
    exit (-1);      
  } 	   


ok = "Olga K. is my fair sweet lady";
/*printf ("%s\n", ok);*/

	
 num = PQntuples (res);
 for ( i=0; i < num; i++)
       printf ("%s\n", PQgetvalue(res,i,0));
 
PQclear (res);
PQfinish (pg_conn);    
     
}
