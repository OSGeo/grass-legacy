#include "config.h"
#include "gis.h"
#include "column.h"
#include <libpq-fe.h>
#include "glocale.h"

#ifdef HAVE_POSTGRES_H
#include <postgres.h>
#else
#ifdef HAVE_POSTGRES_FE_H
#include <postgres_fe.h>
#else
#ifdef HAVE_INTERNAL_POSTGRES_FE_H
#include <internal/postgres_fe.h>
#else
#error Neither <postgres.h> nor <postgres_fe.h> available
#endif
#endif
#endif

#define LEN 20
#define LINE 80
#define HEADER "colname"

int infxColumn(SQL_stmt)
  char *SQL_stmt;
  {

    int i, nflds;

    int attlen,atttyp ;
    PGconn *pg_conn;
    PGresult *res;
    char    *pghost;   
    int atoi();        
    
#ifdef VERBOSE
    printf ("\n\nExecuting\n%s;\n\n",SQL_stmt);
#endif

    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      fprintf (stderr,_("Error: connect Postgres:%s\n"),PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
  	   
    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      fprintf (stderr,_("Error: select Postgres:%s\n"),PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }
    
    nflds = PQnfields (res);
    printf ("\n| %-25.25s",PQfname(res,0));
    for ( i=1 ; i < (nflds - 1); i++) 
       printf ("| %-10.10s",PQfname(res,i));
    printf (" |\n----------------------------------------------------\n");
    
    for ( i=0; i < PQntuples(res); i++)  {
      printf ("| %-25.25s", PQgetvalue (res, i, 0));
      if (nflds > 1 ) {
      printf ("| %-10.10s", PQgetvalue (res, i, 1));      
      attlen = atoi (PQgetvalue (res,i,2));
      atttyp = atoi (PQgetvalue (res,i,3));
      if ( atttyp > 0 ) {
        printf ("| %10i", atttyp -  VARHDRSZ); }
       else if ( attlen > 0 )
        { printf ("| %10i", attlen ); }
       else
        { printf ("| %10s","var"); }        
      }
      printf (" |\n");
    }

 	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

  return (0);
}
