/*
 *   g.select.db
 *
 *
 *   Select an ORACLE SQLPLUS user and passwd to use in conjunction with
 *   subsequent GRASS applications.
 *   jaf 12/28/91
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
	int i, rsCnt, hit, dblen ;
        char *dbpath, *p;  
        char dbstring[1024];
        struct Option *opt1;

	opt1 = G_define_option() ;
	opt1->key        = "user" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES  ;
	opt1->multiple   = NO ;
	opt1->description= "Name/password as in SQL*Plus" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;


	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

/*************** ORACLE driver code begins ***************/

	/* Make sure database is available */

  	dbpath = getenv("ORACLE_HOME");
  	if(!dbpath)
		G_fatal_error("ORACLE_HOME not set");
  	G_setenv("DATABASE", opt1->answer);

}

