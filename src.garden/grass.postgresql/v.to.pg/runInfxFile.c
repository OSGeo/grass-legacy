#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
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
    int mean_vertices = 0;
    
    stat = 1 ;
    i = 1;
    line_cat = 0;

/* Now that the SQL has output CAT value(s) plot vectors with same cat vals */

	/* Initialize graphics stuff */
#ifndef X_DISPLAY_MISSING
	R_open_driver();
	D_setup(0) ;
	R_standard_color(color);
#endif
	Points = Vect_new_line_struct();
        if (2 > Vect_open_old (&P_map, map, mapset))
        { 
                printf (_("Error opening vector map %s in mapset %s\n"),
                map,mapset);
                return -1;
         }
	 build_lookup_tables (&P_map);

	/* Read SQL output and draw vectors */
    
    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      printf (_("Error Connecting to Postgres:%s\n"),PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
	    
	    if (!table_string) table_string = map_string;
	    if (!key_string) key_string = map_string;
	    		
	    if (!strncmp(vtype_string,"area",4)) {

	    	if (!to_postgis) {
			 snprintf (buf1,128,"CREATE TABLE %s_bnd (%s int4, num int2, ex bool, boundary polygon)",
				table_string, key_string);
		} else {
			 snprintf (buf1,128,"CREATE TABLE %s_mpoly (%s int4, num int2)",
				table_string, key_string);    
		}
	    } else {
		if (!to_postgis) {
	    		snprintf (buf1,128,"CREATE TABLE %s_arc (%s int4, num int2, segment path)",
	    			table_string, key_string);
		} else {
			snprintf (buf1,128,"CREATE TABLE %s_mstring (%s int4, num int2)",
	    			table_string, key_string);
		}
	    }
		
	    if (verbose) printf ("Executing\n%s;\n\n",buf1);
	    
	    res = PQexec (pg_conn, buf1);
	    	if ( strlen(PQerrorMessage(pg_conn)) ) {
      			printf (_("Could not create new table:%sExiting.\n"),PQerrorMessage(pg_conn)); 
      			PQfinish(pg_conn);
      			exit (-1);      
    		}

    	    PQclear(res);
	    
	if (to_postgis) {
		    if (!strncmp(vtype_string,"area",4)) {


			snprintf (buf1,128,"SELECT AddGeometryColumn ('%s', '%s_mpoly', 'grass_poly', -1,'POLYGON',2)", 
			G_getenv("PG_DBASE"), table_string);

	    	    } else {
			snprintf (buf1,128,"SELECT AddGeometryColumn ('%s', '%s_mstring', 'grass_line', -1, 'LINESTRING',2)",
			G_getenv("PG_DBASE"), table_string);

	     	    }
	

	    if (verbose) printf ("Executing\n%s;\n\n",buf1);
	    
	    res = PQexec (pg_conn, buf1);
	    	if ( strlen(PQerrorMessage(pg_conn)) ) {
      			printf (_("Could not add geometry column:%sExiting.\n"),PQerrorMessage(pg_conn)); 
      			PQfinish(pg_conn);
      			exit (-1);      
    		}

    	    PQclear(res);
	
	}
	
if (!total_import){  
    
    if (verbose) printf ("Executing\n%s;\n\n",SQL_stmt);
	      
    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf (_("Error Selecting from Postgres:%s\n"),PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }
      if (verbose) printf (_("%d Rows\n"),PQntuples(res));
    for ( i=0; i < PQntuples(res); i++)  {
      strncpy (buf1, PQgetvalue (res, i, 0),1024);
      line_cat = atoi (buf1);

  /* use ascii buffer to Postgres until MSB / LSB issues can be determined */
  /* cfa 11/98    */
      
      stat = plotCat(map,mapset,Points,line_cat, &P_map, fillcolor, pg_conn);
      if (stat == -1) {
        printf (_("Error plotting category %d\n"),line_cat);
        exit (-1);
      } 

    }
 

} else {
      stat = plotCat(map,mapset,Points,line_cat, &P_map, fillcolor, pg_conn);
      if (stat == -1) {
        printf (_("Error plotting category %d\n"),line_cat);
        exit (-1);
      } 
} /*end if !total_import*/
    
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */

    PQfinish(pg_conn);
    /* close connection to database */

        Vect_destroy_line_struct (Points);
        Vect_close(&P_map) ;
if (verbose){
if (total_vects) mean_vertices = floor(total_vertices/total_vects);
	printf (_("Total number of vectors inserted = %d, with average of %d vertices\n"), 
	total_vects, mean_vertices);
}
#ifndef X_DISPLAY_MISSING
        R_close_driver();
#endif
        exit(stat);
}

