#include "gis.h"
#include "stats.h"
#include "infx.h"
#include <libpq-fe.h>
//---------------A.Sh 12.99


infxStats(tab, col, freq, where, verbose)
  char *tab;
  char *col;
  char *freq;
  char *where;
  char *verbose;
  {
    int i, nflds, can_avg_col = 1;
    static char		SQL_stmt[1024];
    char buf[1024];
    PGconn *pg_conn;
    PGresult *res;
    char    *pghost;
    char wherecl[500] = " ";
    i = 0;

  if ( where ) sprintf (wherecl," WHERE %s ",where);

    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      fprintf (stderr, "Error: select Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
    
 if (freq )  {
  sprintf (SQL_stmt,"SELECT count(*) as count, %s from %s %s 
         group by %s order by count(*)"
         ,col,tab,wherecl,col);  }
  else 
  { sprintf (SQL_stmt, "select avg(%s) from %s where oid is null",col,tab,col);
    res = PQexec (pg_conn, SQL_stmt );
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) 
       can_avg_col = 0; 
 
    PQclear (res);     
    if ( can_avg_col ) {
       sprintf (SQL_stmt,"SELECT min(%s) as Min, max(%s) as Max, avg(%s) as Mean
       FROM %s %s",col,col,col,tab,wherecl);  }
    else
       {sprintf (SQL_stmt,"SELECT min(%s) as Min, max(%s) as Max
       FROM %s %s",col,col,tab,wherecl);  }     
    }
    
    
   if (verbose)
      printf ("\n\nExecuting\n%s;\n\n",SQL_stmt);


    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf ("Error: connect Postgres:%s\n",PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }

    
    nflds = PQnfields (res);
    if (freq) {
    printf ("%10s, %-25.25s\n","Count",col);
    printf ("-------------------------------------\n");
    for ( i=0; i < PQntuples(res); i++) 
      { printf ("%10s, %-25.25s\n", PQgetvalue (res, i, 0), 
         PQgetvalue (res, i, 1)); }  
    } else {
    printf ("%10s, %10s, %10s\n","Min","Max","Mean");
    printf ("-------------------------------------\n");
    for ( i=0; i < PQntuples(res); i++) {
      if (can_avg_col)
        {strcpy (buf, PQgetvalue (res, i, 2));}
      else
        {strcpy (buf, "undefined");}
      printf ("%10s, %10s, %10s\n", PQgetvalue (res, i, 0), 
         PQgetvalue (res, i, 1), buf);
     }
    }
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

        printf("\n");


	return 0;

		
}


