/* d.vect.pg  */
/* runInfxFile.c    */
/* major modifications 11/1998 Carl Anderson */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libpq-fe.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#include "dbvect.h"
#include "glocale.h"


int runInfxFile(SQL_stmt, map, mapset, color, fillcolor )
  char *SQL_stmt, *map, *mapset;
  int color, fillcolor ;
  {

    int i;
    int line_cat;		/* vector cat values from db 	*/ 
    int stat;			/* return value from plot2 	*/

    char buf1[1024];		/* value in key column		*/
    struct line_pnts *Points ;
    struct Map_info P_map;

    PGconn*	pg_conn;
    PGresult*	res;
    char	*pghost;
   

    stat = 1 ;
    i = 1;
    line_cat = 0;

/* Now that the SQL has output CAT value(s) plot vectors with same cat vals */

	/* Initialize graphics stuff */

	R_open_driver();
	D_setup(0) ;
	R_standard_color(color);
	Points = Vect_new_line_struct();
        if (2 > Vect_open_old (&P_map, map, mapset))
        { 
                printf (_("Error opening vector map %s in mapset %s\n"),
                map,mapset);
                return -1;
         }
	 build_lookup_tables (&P_map);

	/* Read SQL output and draw vectors */
    printf ("Executing\n%s;\n\n",SQL_stmt);
    
    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      printf (_("Error Connecting to Postgres:%s\n"),PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
  	      
    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf (_("Error Selecting from Postgres:%s\n"),PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }
      printf (_("%d Rows\n"),PQntuples(res));
    for ( i=0; i < PQntuples(res); i++)  {
      strcpy (buf1, PQgetvalue (res, i, 0));
      line_cat = atoi (buf1);

  /* use ascii buffer to Postgres until MSB / LSB issues can be determined */
  /* cfa 11/98    */
      
      stat = plotCat(map,mapset,Points,line_cat, &P_map, fillcolor);
      if (stat != 0) {
        printf (_("Error plotting category %d\n"),line_cat);
        exit (-1);
       } 
    }
 	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

        Vect_destroy_line_struct (Points);
        Vect_close(&P_map) ;

        R_close_driver();

        exit(stat);
}

