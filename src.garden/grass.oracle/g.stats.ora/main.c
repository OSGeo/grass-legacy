/*
 *   g.stats.db
 *
 *
 *   Generate simple staistics (MIN, MAX, MEAN, MODE and FREQ),
 *   of selected database column for a specified table
 *   in the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/27/91
 */

#include "gis.h"
#include "infx.h"
#include "stats.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *tabname, *colname;  
    char buf[1024];
    char sqlFile[100];
    int stat;

    struct Query *query1;
    struct Option *opt1, *opt2, *opt3;
    struct Flag *flag;


	stat = 0;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	opt1 = G_define_option() ;
	opt1->key        = "table" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES  ;
	opt1->multiple   = NO ;
	opt1->description= "Name of table" ;

	opt2 = G_define_option() ;
	opt2->key        = "column" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES  ;
	opt2->multiple   = NO ;
	opt2->description= "Column in [table] which is numeric in type." ;

	sprintf(sqlFile,"/tmp/%d.sql",getpid() );

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.db to identify a current database.\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
	    exit(-1);

	/* Initialze SQL query structure */

/*************** INFX driver code begins ***************/
        
	stat = chktype(opt1->answer,opt2->answer,sqlFile);
	exit(0);

}





buildSQL(fp,tabname,colname)
    FILE *fp;
    char *tabname, *colname;
{
	fprintf(fp,SELECT,colname,colname,colname,colname);
	fprintf(fp,FROM,tabname);
	fprintf(fp,WHERE,colname);
        fprintf(fp,"quit\n");
}

