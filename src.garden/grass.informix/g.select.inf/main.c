/*
 *   g.select.inf
 *
 *
 *   Select an SQL database to use in conjunction with
 *   subsequent GRASS applications.
 *   jaf 12/28/91
 */

#include <stdio.h>
#include "gis.h"
#define MAIN
#define RS ":"

main(argc, argv)
int argc ;
char **argv ;
{
  char  *getenv(), *strtok(), *strcat() ;
	int i, rsCnt, hit, dblen ;
        char *dbpath, *p;  
        char dbstring[1024];
	struct Option *opt1;
	struct Flag *list ;



        list = G_define_flag();
        list->key               = 'l';
        list->description       = "Use -l for a list of databases in current path.";


	opt1 = G_define_option() ;
	opt1->key        = "database" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO  ;
	opt1->multiple   = NO ;
	opt1->description= "Name of existing database" ;

        hit = 1;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;


	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

/*************** INFX driver code begins ***************/

	/* Make sure database is available */

  dbpath = getenv("DBPATH");
  if(!dbpath)
	G_fatal_error("DBPATH not set");

  if(list->answer) {
  	p = G_malloc (strlen(dbpath) + 2);
        printf("The following databases are in your current path:\n");
  	strcpy (p, dbpath);
  	dbpath = p;
  	do {
     		rsCnt = strcspn(dbpath,RS);
     		for(i=0; i<rsCnt; i++) 
         		dbstring[i] = (*dbpath++);
     			dbstring[i] = NULL;
     			listdb(dbstring); 
  	} while (*dbpath++);
	exit(0) ;
	}


  p = G_malloc (strlen(dbpath) + 2);
  strcpy (p, dbpath);
  strcat (dbpath=p,RS);

  do {
     rsCnt = strcspn(dbpath,RS);
     for(i=0; i<rsCnt; i++) 
         dbstring[i] = (*dbpath++);
     dbstring[i] = NULL;
     hit = getdbname(dbstring,opt1->answer); 
  } while (*dbpath++ && hit != 0);

  if (hit == 0)
   {
      G_setenv("DATABASE", opt1->answer);
   }
   else
    {
	G_warning("Database not available.");
		exit(-1) ;
    }


}

