/*added pg_type selection for site_list --alex 03/2002*/

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "site.h"
#include <stdio.h>
#include <libpq-fe.h>
#include "dbsite.h"
#include "glocale.h"

int runInfxFile(SQL_stmt, map,  plotargs )
  char *SQL_stmt, *map, *plotargs[];
  {
    FILE *fpout = NULL;
    int i,nflds,err;
    int retval;
    int color;
    int size;
    char *icon;
    char buf1[1024] = "";
    char buf2[1024] = "";
    char *buf3;
    float x;
    float y;
    PGconn *pg_conn;
    PGresult *res;
    char    *pghost;
    Site *site;
    int ftype=0;
    
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

    if (map){
         
     if ((fpout = G_fopen_sites_new (map)) == NULL)
    	
	{
      		sprintf (buf1, _("Cannot open %s"), map);
      		G_fatal_error (buf1);
    	}

    }	
	
		
#ifdef VERBOSE
    printf ("\n\nExecuting\n%s;\n\n",SQL_stmt);
#endif

    pghost = G__getenv("PG_HOST");
        
    pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    if (PQstatus (pg_conn) == CONNECTION_BAD) {
      printf (_("Error: Selecting from Postgres:%s\n"),PQerrorMessage(pg_conn));
      PQfinish(pg_conn);
      exit (-1); 
    }
  	   
    res = PQexec (pg_conn, SQL_stmt);
    if ( PQresultStatus (res) != PGRES_TUPLES_OK ) {
      printf (_("Error: Connecting to Postgres:%s\n"),PQerrorMessage(pg_conn)); 
      PQfinish(pg_conn);
      exit (-1);      
    }
    
    nflds = PQnfields (res);
    
      if ( nflds == 3 ) {
      ftype = PQftype(res,2);
      switch (ftype) {
			
			case 1042:
			break;
			
			case 1043:
			break;
			
			case 18:
			break;
						
			case 25:
			break;


			case 20:
			break;

			case 21:
			break;

			case 23:
			break;
			
			case 700:
			break;
			
			case 701:
			break;
			
			default:
			printf("Can not use the field type id %d for category\n",ftype);
		}
	}

    for ( i=0; i < PQntuples(res); i++)  {
      strcpy (buf1, PQgetvalue (res, i, 0));
      strcpy (buf2, PQgetvalue (res, i, 1));
      x = atof (buf1);
      y = atof (buf2);
  /* use ascii buffer to Postgres until MSB / LSB issues can be determined */
  /* cfa 11/98    */

      retval = plotsite (x,y,icon,size);
      if (retval != 0) {
      	printf(_("Display error(exiting..)\n"));
      	exit (-1);
      }
      if (map ) {
	if (ftype) {
		
		strcpy (buf3, PQgetvalue (res, i, 2));

		switch (ftype) {

/*char, bpchar,varchar,text*/
			case 18:
			site = G_site_new_struct (-1, 2, 1, 0);
			if (strlen(buf3) == 0) strcpy(buf3,"no data");
			G_strncpy (site->str_att[0], buf3, strlen(buf3));
			break;
			case 1042:
			site = G_site_new_struct (-1, 2, 1, 0);
			if (strlen(buf3) == 0) strcpy(buf3,"no data");
			G_strncpy (site->str_att[0], buf3, strlen(buf3));
			break;
			case 1043:
			site = G_site_new_struct (-1, 2, 1, 0);
			if (strlen(buf3) == 0) strcpy(buf3,"no data");
			G_strncpy (site->str_att[0], buf3, strlen(buf3));
			break;
			case 25:
			site = G_site_new_struct (-1, 2, 1, 0);
			if (strlen(buf3) == 0) strcpy(buf3,"no data");
			G_strncpy (site->str_att[0], buf3, strlen(buf3));
			break;
/*int4, int2, int8*/
			case 20:
			site = G_site_new_struct (CELL_TYPE, 2, 0, 0);
			sscanf(buf3,"%d",&site->ccat);
			break;
			case 21:
			site = G_site_new_struct (CELL_TYPE, 2, 0, 0);
			sscanf(buf3,"%d",&site->ccat);
			break;
			case 23:
			site = G_site_new_struct (CELL_TYPE, 2, 0, 0);
			sscanf(buf3,"%d",&site->ccat);
			break;
/*float4, float8*/
			case 700:
			site = G_site_new_struct (FCELL_TYPE, 2, 0, 0);
			sscanf(buf3,"%f", &site->fcat);
			break;
			case 701:
			site = G_site_new_struct (FCELL_TYPE, 2, 0, 0);
			sscanf(buf3,"%f", &site->fcat);
			break;

			default:
			site = G_site_new_struct (-1, 2, 0, 0);
		}
	   } else 
	   	site = G_site_new_struct (-1, 2, 0, 0);
	
	site->east = x;
	site->north = y;
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
