#include <stdio.h>
/*#include "libpq-fe.h"*/
#include "/usr/lib/pgsql/include/libpq-fe.h"
#include <gis.h>
#include "sql.h" 


void exit_nicely(PGconn* conn)
{
  PQfinish(conn);
  exit(1);
}

main(argc, argv)
     int argc ;
     char **argv;



{
  /* Database variables */
  char pghost[30], pgport[10], *pgoptions, *pgtty;
  char dbName[30];
  int nFields;
  int i,j,dummy;
  double easting, easting_range=500;
  double northing, northing_range=500;
  double min_distance;
  char *min_east, *min_north;
  int count,fcount, tcount;
  char line[300];
  int button=0;
  struct Postgres postgres;
  PGconn* conn;
  PGresult* res;
 /* Get values from ~/.pg_grc */ 
 
 
  get_post_head(&postgres);
  puts("back");
  strcpy(pghost,postgres.host);
  strcpy(pgport,postgres.portnum);
  pgoptions = NULL;
  pgtty = NULL;
  
  
  

  strcpy(dbName,postgres.database);
  printf("Using %s as database, table %s\n",dbName,postgres.def_table);
  printf("host: %s port: %s\n",pghost,pgport);
  G_gisinit(argv[0]);
  R_open_driver(); 
  D_setup(0); 
  
  conn=PQsetdb(pghost,pgport,pgoptions,pgtty,dbName);
  if (PQstatus(conn) == CONNECTION_BAD )
    { fprintf(stderr,"Connection to database %s failed",dbName);
    fprintf(stderr,"%s",PQerrorMessage(conn));
      exit_nicely(conn);
    }
  



  for (;;button==3) {  
  printf("Click on a point (or button 3 to exit): \n");
  button=getcoord(&easting, &northing);
  if (button==3) return(3);
  printf("easting %f, northing %f\n",easting,northing);
  printf("--------Nearby Holes----------------------------------\n");
  /*  Find closest match to point and print */
  sprintf(line,"select easting, northing from %s where easting > %f and easting < %f and northing > %f and northing < %f order by easting",postgres.def_table, easting-easting_range,easting+easting_range,northing-northing_range,northing+northing_range);
  /* printf("%s\n",line);*/
  res=PQexec(conn, line);
  
  if (PQntuples(res) == 0 ) printf("Nothing available\n");
  else {
  nFields=PQnfields(res);
  for (i=0; i < nFields; i++){
    printf("%-15s",PQfname(res,i));
    
}
  printf("\n");
  for (i=0; i < PQntuples(res); i++) {

    for(j=0; j < nFields; j++) {
      printf("%-15s",PQgetvalue(res,i,j));
    }
    printf("\n");}
  
  /* Rework, get list just coordinates. Find closest match
     then get all data from database. */
  min_distance=700; /* good start value, search doesn't extend beyond. */

  for (i=0 ; i < PQntuples(res); i++) {
    if (min_distance > distance_from_pointer(easting,northing,PQgetvalue(res,i,0),PQgetvalue(res,i,1))) {
      min_distance=distance_from_pointer(easting,northing,PQgetvalue(res,i,0),PQgetvalue(res,i,1));
      
      
     min_north=PQgetvalue(res,i,1);
     min_east=PQgetvalue(res,i,0);
     
    }
  }

printf("coordinates of site -- east %s, north %s\n",min_east,min_north);
sprintf(line,"select * from %s where easting=%s and northing=%s",postgres.def_table,min_east,min_north);
printf("query -- %s\n",line);

res=PQexec(conn,line);
nFields=PQnfields(res);
/*
  for (i=0; i < nFields; i++){
    printf("%-15s",PQfname(res,i));
    
}
  printf("\n");
  for (i=0; i < PQntuples(res); i++) {

    for(j=0; j < nFields; j++) {
      printf("%-15s",PQgetvalue(res,i,j));
    }
    printf("\n");}
    */

  

  puts("--------------------------------------------------");

  count=0;
  for (fcount=0 ; fcount < nFields; dummy++)
    { for (count=0; count < 5 ; count++) 
      { 
	
	if (fcount < nFields) printf("%-15s",PQfname(res,fcount)) ;
	fcount++;}
     printf("\n");
    for (tcount=fcount-5 ;tcount< fcount; tcount++)
      {
	if (tcount < nFields ) printf("%-15s",PQgetvalue(res,0,tcount)); }
    printf("\n");
    }
      
  }
PQclear(res);	       
  } /* inside button loop */			    

  /* Next Test, Same results, check for closest match */
  /* 1st field should be easting, second field should be northing */

  /*printf("Pointer at %f e,%f n\nFirst Match at %s %s\nDifference %f\n",easting,northing,PQgetvalue(res,0,0),PQgetvalue(res,0,1),distance_from_pointer(easting,northing,PQgetvalue(res,0,0),PQgetvalue(res,0,1)));*/
}






