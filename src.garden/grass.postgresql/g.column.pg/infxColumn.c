#include "gis.h"
#include "column.h"
#include <libpq-fe.h>
#include <postgres.h>

#define LEN 20
#define LINE 80
#define HEADER "colname"

infxColumn(SQL_stmt)
  char *SQL_stmt;
  {
    FILE *fp;
    int i, j, nflds;
    char cname[100];
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
      fprintf (stderr,"Error: connect Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
  	   
    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      fprintf (stderr,"Error: select Postgres:%s\n",PQerrorMessage(pg_conn)); 
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

parseType(cname, ctype, clength)
  char *cname;
  int ctype, clength;

{
	char *vtype;


	switch(ctype) {
		case 0: vtype  = "char"; break;
		case 1: vtype  = "smallint"; break;
		case 2: vtype  = "integer"; break;
		case 3: vtype  = "unknown"; break;
		case 4: vtype  = "unknown"; break;
		case 5: vtype  = "decimal"; break;
		case 6: vtype  = "unknown"; break;
		case 7: vtype  = "date"; break;
		case 256: vtype = "char (no nulls)"; break;
		case 257: vtype = "smallint (no nulls)"; break;
		case 258: vtype = "integer (no nulls)"; break;
		case 259: vtype = "unknown (no nulls)"; break;
		case 260: vtype = "unknown (no nulls)"; break;
		case 261: vtype = "decimal (no nulls)"; break;
		case 262: vtype = "serial (no nulls)"; break;
		default: return; break;
	}

	/* Informix identifies decimal data types using 2**8x signif digits + precision
	   therefore decimal(5,2)=1282=2**8x5+2
	*/
	if(ctype == 5 || ctype  == 261) 
		fprintf(stdout,"%20s	%20s	%d,%d\n",cname, vtype, clength/256, clength%256);
	 else
		fprintf(stdout,"%20s	%20s	%d\n",cname, vtype, clength);
}
