/*
 *   g.column.inf
 *
 *
 *   Generate list of database columns for a specified table
 *   in the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/26/91
 */

#include "gis.h"
#include <stdio.h>
/* #include "column.h" */
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *tabname;  
    char sqlFile[100];

    struct Option *opt1;
    struct Flag *flag;


	opt1 = G_define_option() ;
	opt1->key        = "table" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES  ;
	opt1->multiple   = NO ;
	opt1->description= "Name of table in currently selected  database" ;

	flag = G_define_flag();
	flag->key		= 'v';
	flag->description	= "Use v for a verbose listing of columns.";



	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	sprintf(sqlFile,"/tmp/%d.sql",getpid());

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.pg to identify a current database.\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
	    exit(-1);


	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (sql).\n");
	    exit(-1);
           }

	if (flag->answer) {
		fprintf(fp,"SELECT (pg_attribute.attname,pg_type.typname)\n");
		fprintf(fp,"where  pg_class.relname = \"%s\"\n",opt1->answer);
		fprintf(fp,"and    pg_attribute.atttypid = pg_type.oid \n");
		fprintf(fp,"and pg_attribute.attrelid=pg_class.oid \n");
		fprintf(fp,"and   pg_attribute.attnum   > 0 \n");
		fprintf(fp,"sort   by attname \n");
	}
	else {
		fprintf(fp,"SELECT (pg_attribute.attname)\n");
		fprintf(fp,"where  pg_class.relname = \"%s\"\n",opt1->answer);
		fprintf(fp,"and pg_attribute.attrelid=pg_class.oid \n");
		fprintf(fp,"and   pg_attribute.attnum   > 0 \n");
		fprintf(fp,"sort   by attname \n");		
	}

        fclose(fp);


/*************** INFX driver code begins ***************/
        
	infxColumn(opt1->answer,flag->answer,sqlFile);

	unlink(sqlFile);
	exit(0);

}

