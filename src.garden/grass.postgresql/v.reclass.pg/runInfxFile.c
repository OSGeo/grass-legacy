/*	A.Sh. jan'00
*/
#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include <libpq-fe.h>

#define TRUE 1
#define FALSE 0

runInfxFile(SQL_stmt, input,output, vtype, disolve )
  char *SQL_stmt, *input, *output, *vtype;
  int disolve;
  {
    FILE *fpout;
    int i,TMP;
    char *tmpfile_rules;

    char sysbuf[1024] = "";
    char buf1[1024] = "";
    char buf2[1024] = "";

    PGconn *pg_conn;
    PGresult *res;
    char    *pghost;


    TMP=0;

#ifdef VERBOSE
    printf ("\n\nExecuting\n%s;\n\n",SQL_stmt);
#endif

    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      printf ("Error: Selecting from Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
  	   
    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf ("Error: Connecting to Postgres:%s\n",PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }

	tmpfile_rules = G_tempfile() ;
	
	if((fpout = fopen(tmpfile_rules,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (rules)\n");
	    exit(-1);
           }

    for ( i=0; i < PQntuples(res); i++)  {
      strncpy (buf1, PQgetvalue (res, i, 0),1024);
      strncpy (buf2, PQgetvalue (res, i, 1),1024);
       
       fprintf(fpout,"%s=%s\n", buf1, buf2);

   
      }
	fprintf(fpout,"end");
	fclose(fpout);
	if (!output) {
	    output="tmp.recl" ;
	    TMP = TRUE;
	}
	
	if (disolve)
		snprintf(sysbuf,1024,"v.reclass input=%s output=%s type=%s -d < %s\n",
		input, output, vtype, tmpfile_rules);
	  else 
		snprintf(sysbuf,1024,"v.reclass input=%s output=%s type=%s < %s\n",
		input, output, vtype, tmpfile_rules);
	system(sysbuf);
	
	if (TMP==TRUE) {
	snprintf(sysbuf,1024,"d.vect %s\n",output);
	system(sysbuf);
	}
	
	unlink(tmpfile_rules);
 	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */


 
	if (TMP==TRUE) {
		G_remove("dig","tmp.recl") ;
		G_remove("dig_cats","tmp.recl") ;
		G_remove("dig_misc","tmp.recl") ;
		G_remove("hist","tmp.recl") ;
		G_remove("dig_plus","tmp.recl") ;
		G_remove("dig_att","tmp.recl") ;
	}
    return(0);
}
