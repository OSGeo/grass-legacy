/*
 *   g.select.inf
 *
 *
 *   Select an SQL database to use in conjunction with
 *   subsequent GRASS applications.
 *   jaf 12/28/91
 #--------------------A.Sh -12.99

 * g.select.pg  
 *
 * Selects a Postgres database to use
 * J.Soimasuo 7th March 1994
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
        struct Option* pghost;
        struct Flag *list ;

        list = G_define_flag();
        list->key               = 'l';
        list->description       = "Use -l flag for list of databases.";

	pghost = G_define_option() ;
	pghost->key        = "host" ;
	pghost->type       = TYPE_STRING ;
	pghost->required   = NO  ;
	pghost->multiple   = NO ;
	pghost->description= "Postgres server name:" ;
 
 	opt1 = G_define_option() ;
	opt1->key        = "database" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO  ;
	opt1->multiple   = NO ;
	opt1->description= "Postgres database name:" ;

        hit = 1;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;


	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

/*************** INFX driver code begins ***************/

	/* Make sure database is available */

  if ( pghost->answer )
       G_setenv("PG_HOST", pghost->answer);

  if(list->answer) {
        printf("The following databases are in the Unix catalogue:\n");

	listdb(pghost->answer);
	exit(0) ;
	}

  if(!opt1->answer){
	  opt1->answer=getenv("USER");
  }

     hit=getdbname(opt1->answer);
  
  if (hit == 0)
   {
      G_setenv("PG_DBASE", opt1->answer);

   }
   else
    {
	G_warning("This database does not exist.");
       	printf("The following databases are in the Unix catalogue:\n");
	listdb(pghost->answer);
	exit(-1) ;
    }
   
}

