/*A.Sh -12.99*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <libpq-fe.h>
#include "gis.h"

int getdbname(dbtemp)
char *dbtemp;

{

PGconn     *pg_conn;
PGresult   *res;

char       *pghost;

int        i;
int  	   ok=1;
int	   rec_num;

pghost = (char*) G__getenv("PG_HOST");

pg_conn = PQsetdb(pghost, NULL, NULL, NULL, "template1");


if (PQstatus (pg_conn) == CONNECTION_BAD) {
    fprintf (stderr, "Error: select Postgres:%s\n",
       PQerrorMessage(pg_conn));
    exit (-1);
  }
  
 res  = PQexec(pg_conn, "select datname from pg_database");
 
 if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
    fprintf (stderr,"Error: connect Postgres:%s\n",PQerrorMessage(pg_conn));
    PQfinish(pg_conn);
    exit (-1);      
  } 	   
 
 rec_num = PQntuples (res);
 for (i=0; i < rec_num; i++) {
       ok = strcmp (dbtemp, PQgetvalue(res,i,0));
       if ( !ok )  break;
    } 
    
PQclear (res);
PQfinish (pg_conn);    
return (ok);
}

