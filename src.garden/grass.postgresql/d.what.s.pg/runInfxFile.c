/* d.what.s.pg
   runInfxFile.c
   run query in loop or run query once
   major modifications cfa 11/98    */
//-------------A.Sh. Jan.00--Output switch vertical/horizontal
   
#include "gis.h"
#include "infx.h"
#include <libpq-fe.h>


runqry(SQL_stmt, pts, print_out)
	char *SQL_stmt;
        struct Sql *pts;
	char *print_out;
{
	char buf[1024];
	char ch ;
	char sqlcmd[1024] ;
	int i,j,nrows,nfields;
    	PGconn *pg_conn;
   	PGresult *res;
    	char    *pghost;
	i = 1;

	sprintf(sqlcmd, 
          "%s @ '(%f,%f,%f,%f)'::box",SQL_stmt,
            pts->minX,pts->minY,pts->maxX,pts->maxY);
	/* use Postgres graphic operators 
	     @ operator test point in box 
	  cfa 11/98   */
    
    fprintf (stderr,"\n\nExecuting\n%s;\n clause  @ '( )'::box addded autonmatically.\n\n",sqlcmd);
    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      printf ("Error: select Postgres:%s\n",PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
  	   
    res = PQexec (pg_conn, sqlcmd);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf ("Error: Connecting to Postgres:%s\n",PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }

    nfields = PQnfields(res);
    nrows = PQntuples(res);
    if (nrows == 1 && !strncmp(print_out,"v",1)) {
		for ( j=0; j < nfields; j++) {
			strcpy (buf, PQgetvalue (res, 0, j));
			fprintf(stderr,"%10s I %s\n",PQfname(res,j),buf);	      
		}
	} else if (nrows){
		printf("%s",PQfname(res,0));      
    		for (j=1; j < nfields; j++ ) {
      			printf(",%s",PQfname(res,j));
    		}
    		printf ("\n");
    		for ( i=0; i < nrows; i++)  {
      			for ( j=0; j < nfields; j++) {
      				strcpy (buf, PQgetvalue (res, i, j));
   	 			printf ("%s,",buf);  
      			}
      			printf ("\n");
		}
    	} 
 
    fprintf (stderr,"\n%d rows selected\n",nrows);
    	
    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

    PQfinish(pg_conn);
    /* close connection to database */

	return 0 ;
}


getVal(curval, pts)
	int curval;
        struct Sql *pts;
{
        switch (curval) {
        	case 1:
			return (pts->centX);
			break;
                case 2:
                        return (pts->centX);
			break;
                case 3:
                        return (pts->centY);
			break;
                case 4:
                        return (pts->centY);
			break;
                case 5:
                        return (pts->rad2);
			break;
	}

}

runInfxFile(SQL_stmt, str_dist, print_out)
	char *SQL_stmt;
 	char *str_dist;
	char *print_out;
{
        int    stat,button;
        double searchdist = 0.0 ;
        struct Sql *pts;
        double atof () ;
        
        /* Initialze SQL query structure        */
        pts = (struct Sql *)G_malloc(sizeof(struct Sql)) ;
        G_zero (pts, sizeof(struct Sql)) ;                         
        
        searchdist =  atof (str_dist);
        if ( !searchdist ) { 
          printf ("ошибка преобразования %s, есть %f \n"
          ,str_dist, searchdist); 
          exit (-1); 
        }
/* help I always screw up atof functions ---  look closely and fix it */

        (double) pts->distance = (double) searchdist;
        
        R_open_driver();
        D_setup(0);
        do
        {
                button=getArea(pts);
                if (button != 3)
                       stat = runqry(SQL_stmt, pts, print_out);

        } while (button != 3);

        R_close_driver();

        return (stat);
   
}
