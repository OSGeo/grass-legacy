//-----------changed to Site API, Dec.99. A.Sh.

#include <stdlib.h>
#include "gis.h"

#include <stdio.h>
#include <libpq-fe.h>
#include "dbsite.h"

#define TRUE 0
#define FALSE 1

runInfxFile(SQL_stmt, map,  plotargs )
  char *SQL_stmt, *map, *plotargs[];
  {
    FILE *fp, *fpin, *fpout;
    int i,nflds,err;
    int retval;
    int color;
    int size;
    char *icon;
    char sysbuf[1024];
    char buf1[1024] = "";
    char buf2[1024] = "";
    char *buf3;
    float x;
    float y;
    PGconn *pg_conn;
    PGresult *res;
    char    *pghost;
    Site *site;
    int c = 0;
    
    i = 1;
    retval  = 0 ;
    buf3 = G_malloc (1024 * sizeof (char));
    err=1;
  


    color = D_translate_color(plotargs[0]);
    icon = plotargs[1];
    size = atoi(plotargs[2]);

    R_open_driver();
    D_setup(0) ;
    R_standard_color(color);

    if (map)	fpout=G_fopen_sites_new(map);

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
    
    nflds = PQnfields (res);
    for ( i=0; i < PQntuples(res); i++)  {
      strcpy (buf1, PQgetvalue (res, i, 0));
      strcpy (buf2, PQgetvalue (res, i, 1));
      x = atof (buf1);
      y = atof (buf2);
  /* use ascii buffer to Postgres until MSB / LSB issues can be determined */
  /* cfa 11/98    */
  
      if ( nflds == 3 ) {
      
      strcpy (buf3, PQgetvalue (res, i, 2));
      if (strlen(buf3) == 0) strcpy(buf3,"no data");
      }
      retval = plotsite (x,y,icon,size);
      if (retval != 0) {
      	printf("Display error(exiting..)\n");
      	exit (-1);
      }
      if (map) {
//-A.Sh
      	site = G_site_new_struct (-1, 2, 1, 0);
	site->east = x;
	site->north = y;
	G_strncpy (site->str_att[0], buf3, strlen(buf3));
      	G_site_put(fpout,site);
	G_site_free_struct (site);
	}
    }
 	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

    if (map) fclose(fpout);

    R_close_driver();

    return(0);
}
