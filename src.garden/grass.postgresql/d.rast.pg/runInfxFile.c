//-----------A.Sh. - 22.12.99

#include <stdlib.h>
#include "gis.h"
#include <stdio.h>
#include <libpq-fe.h>
#include "dbrast.h"

#define TRUE 1
#define FALSE 0

runInfxFile(SQL_stmt, input,output, withlabel)
  char *SQL_stmt, *input, *output;
  int withlabel;
  {
    FILE *fpout;
    int i,TMP;
    char *tmpfile_rules;
    

    char sysbuf[1024];
    char buf1[1024] = "";
    char buf2[1024] = "";
    char buf3[1024] = "";

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
      strcpy (buf1, PQgetvalue (res, i, 0));
      strcpy (buf2, PQgetvalue (res, i, 1));
      	if (withlabel) {
      		strcpy (buf3, PQgetvalue (res, i, 2));
        	fprintf(fpout,"%s = %s %s\n", buf1, buf2, buf3);
      	} else {
		fprintf(fpout,"%s = %s\n", buf1, buf2);
	}
   
    }
	fprintf(fpout,"end");
	fclose(fpout);
	if (!output) {
	    output="tmp.recl" ;
	    TMP = TRUE;
	}
	
	sprintf(sysbuf,"r.reclass input=%s output=%s < %s\n",
		input, output, tmpfile_rules);
	system(sysbuf);
	
	if (TMP==TRUE) {
	printf("Output map name has NOT been given,\nso we try to plot and exit..\n");
	sprintf(sysbuf,"d.rast %s\n",output);
	system(sysbuf);
	}
	
	unlink(tmpfile_rules);
 	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */



    
	if (TMP==TRUE) {
		G_remove("cell","tmp.recl") ;
		G_remove("cats","tmp.recl") ;
		G_remove("cellhd","tmp.recl") ;
		G_remove("hist","tmp.recl") ;
		G_remove("cell_misc","tmp.recl") ;
		G_remove("colr","tmp.recl") ;
	}
    return(0);
}
