/*
 *   g.stats.inf
 *
 *#----------------------A.Sh -12.99
 *   Generate simple staistics (MIN, MAX, MEAN, MODE and FREQ),
 *   of selected database column for a specified table
 *   in the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/27/91
 */

/*  g.stats.pg
      after modifactions in '94
      after modes by Markus Neteler in 8/98
      modifications 11/98
        to support v6.4 of Postgress
        use libpq instead of psql
        use pghost,pgdbase
        allow MODE and FREQ for non numeric types
        allow MIN and MAX for char if text_functions.sql has been run
        allow MIN, MAX, MEAN, MODE, FREQ for numeric types
        seperate FREQ from general stats by flag -f
        use verbose sql statement with -v flag 
*/

#include "gis.h"
#include "infx.h"
#include "stats.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    char *dbname;
    char *tabname, *colname;  
    char buf[1024];
    char sqlFile[100];
    int stat;

    struct Query *query1;
    struct Option *opt1, *opt2, *cond;
    struct Flag *flag, *verb;


	stat = 0;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	opt1 = G_define_option() ;
	opt1->key        = "table" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES  ;
	opt1->multiple   = NO ;
	opt1->description= "Name of the table in the selected database:" ;

	opt2 = G_define_option() ;
	opt2->key        = "column" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES  ;
	opt2->multiple   = NO ;
	opt2->description= "Column in [table]." ;

	cond = G_define_option() ;
	cond->key        = "where" ;
	cond->type       = TYPE_STRING ;
	cond->required   = NO ;
	cond->multiple   = NO ;
	cond->description= "Clause (where) for the query, without WHERE" ;

        flag = G_define_flag();
        flag->key               = 'f';
        flag->description       = "Use -f for frequencies \n\tinstead of min, max, mean.";

        verb = G_define_flag();
        verb->key               = 'v';
        verb->description       = "Use -v for full output.";

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.pg to identify a current database..\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
	    exit(-1);


/*************** INFX driver code begins ***************/
        
	stat = infxStats(opt1->answer,opt2->answer,flag->answer,cond->answer,verb->answer);
	exit(stat);

}


