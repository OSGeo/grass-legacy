                   /* Read option file .pg-grc 
		      Options - database location 
		                default table     */
#include <stdio.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include "sql.h"
#include "/usr/lib/pgsql/include/libpq-fe.h"

void get_post_head(post)
     struct Postgres *post;

{
  char line[80];
  struct passwd *pw;
  uid_t uid;
  gid_t gid;
  
 
  
  FILE *post_init;
  char input[30];
  char arg[30];
  char val[30];
  char *ch;
  
  /*added M.N. 8/98 */
  char *pghost, *pgport, *pgoptions, *pgtty;
  char* dbName;
  PGconn* conn; PGresult* res;
  /* end addition */  
  
  uid=getuid();
  pw=getpwuid(uid);
  sprintf(line,"%s/.pg-grc",pw->pw_dir);
  printf("Home directory: %s",pw->pw_dir);
  post_init=fopen(line,"r");
  if (post_init==NULL) 
     {
       printf("%s/.pg-grc does not exist.\n\n",pw->pw_dir);
       /*exit(8);*/
       /* retrieving parameters for POSTGRES... M.N. 8/98*/
  
       /* begin, by setting the parameters for a backend connection
          if the parameters are null, then the system will try to use 
          reasonable defaults by looking up environment variables or, 
          failing that, using hardwired constants */ 
          pghost = NULL; /* host name of the backend server */ 
          pgport = NULL; /* port of the backend server */ 
          pgoptions = NULL; /* special options to start up the backend server */ 
          pgtty = NULL; /* debugging tty for the backend server */ 
          dbName = "template1";
  
        /* make a connection to the database */
        printf("Trying to retrieve parameters of POSTGRES database myself...\n");
        conn = PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);
  
        /* check to see that the backend connection was successfully made */ 
        if (PQstatus(conn) == CONNECTION_BAD) {
              fprintf(stderr,"Connection to database '%s' failed.", dbName);
              fprintf(stderr,"%s",PQerrorMessage(conn));
              PQfinish(conn);
              exit(8);
        }
        else {
              printf("Connection to POSTGRES o.k.\n");
              strcpy(post->host,"0");
              strcpy(post->portnum,"0");
              strcpy(post->database,"0");
              strcpy(post->def_table,"0");
        }
     }
  while(ch != EOF){
    ch=fgets(input,sizeof(input),post_init);
    if (ch==NULL) break;
    sscanf(input,"%[^:]:%s",arg,val);
    if (strcmp(arg,"host") == 0) {strcpy(post->host,val); }
    if (strcmp(arg,"port") == 0) {strcpy(post->portnum,val);}
    if (strcmp(arg,"database") == 0) {strcpy(post->database,val);}
    if (strcmp(arg,"table") == 0 ){strcpy(post->def_table,val);}
  }
  
  fclose(post_init);
}
