/* SQL statement to site file converter.  
   Reads Database for easting, northing, and hole_id */


#include "gis.h"
#include <stdio.h>
#include "/usr/lib/pgsql/include/libpq-fe.h"
#include "sql.h"
#include <stdlib.h>

void main(argc,argv)
     int argc;
     char **argv;

{
  char line[150];
  char cline[100]; 
  int i,j;
  PGconn* conn;
  PGresult* res;
  char pghost[30], pgport[10], *pgoptions, *pgtty;
  char dbName[30];
  char east[30], north[30];
  double eastd, northd;
  int nFields;
  char* outname;
  FILE *out_fd;
  
  struct Postgres postgres ;
  struct Option *output, *raster, *SQL  ;
  struct Flag *newtable, *verbose;
  
  newtable=G_define_flag();
  newtable->key='t';
  newtable->description="Create new Table";

  verbose=G_define_flag();
  verbose->key='v';
  verbose->description="verbose";


  output=G_define_option();
  output->key="site";
  output->description="Site file to create";
  output->type=TYPE_STRING;
  output->required=YES;
  
  raster=G_define_option();
  raster->key="raster";
  raster->description="Raster map to create";
  raster->type=TYPE_STRING;
  raster->required=NO;

  SQL=G_define_option();
  SQL->key="sql";
  SQL->description="SQL condition";
  SQL->type=TYPE_STRING;
  SQL->required=YES;

  pgoptions = NULL;
  pgtty = NULL;
  G_gisinit(argv[0]);
  if (G_parser(argc,argv)) exit(1) ;


  puts("SQL -> site file v0.1a");

  get_post_head(&postgres);
  
  strcpy(pghost,postgres.host);
  strcpy(pgport,postgres.portnum);
  strcpy(dbName,postgres.database);
  
  printf("Using %s as database, table %s\n",dbName,postgres.def_table);
  printf("host: %s port: %s\n",pghost,pgport);
  conn=PQsetdb(pghost,pgport,pgoptions,pgtty,dbName);

  
  if (PQstatus(conn) == CONNECTION_BAD )
    { fprintf(stderr,"Connection to database %s failed",dbName);}

  outname=output->answer;
  out_fd=G_fopen_sites_new(outname);
  
  sprintf(line,"select * from %s where %s",postgres.def_table,SQL->answer);

 if (verbose !=NULL) printf("SQL condition -> %s\n\n",SQL->answer);  
 
  res=PQexec(conn,line);
  nFields=PQnfields(res); 
  if (PQntuples(res) != 0) {
    for (i=0; i < PQntuples(res); i++) {
      sprintf(east,"%s",PQgetvalue(res,i,0));
      sprintf(north,"%s",PQgetvalue(res,i,1));
      
     if(verbose != NULL) printf("Site %d -> %f, %f, %s\n",i,atof(east),atof(north),PQgetvalue(res,i,2)); 
      G_put_site(out_fd,atof(east),atof(north),PQgetvalue(res,i,2));
    }
    if (newtable->answer != NULL) {
      sprintf(line,"select *  into table %s from %s where %s",output->answer,postgres.def_table,SQL->answer); res=PQexec(conn,line);
      if (verbose != NULL) printf("Creating new table %s\n",output->answer);}
      if (raster->answer != NULL) 
      { printf ("So you want a raster eh?\n");
      sprintf(cline,"s.to.rast -v input=%s output=%s",output->answer,raster->answer);
      if (verbose != NULL) printf ("Going to execute - %s\n",cline);
      system (cline);
	}
  }
    else printf("No sites matched the condition\n");
  
}












