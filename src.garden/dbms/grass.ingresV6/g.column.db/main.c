/*
 *   g.column.db
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
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *tabname;  
    char sqlFile[100], buf[100];

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
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.db to identify a current database.\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
	    exit(-1);


	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file (sql).\n");
	    exit(-1);
           }

	if (flag->answer) 
	{
		fprintf(fp,"SELECT DISTINCT ");
		fprintf(fp,"column_name, column_datatype, column_length\n");
        }
	else 
	{
		fprintf(fp,"SELECT DISTINCT column_name\n");
        }
	fprintf(fp,"FROM iicolumns\n"); 
	fprintf(fp,"WHERE table_name='%s'\n",opt1->answer);
	fprintf(fp,"ORDER by column_name\n");
	fprintf(fp,"\\g\n");
        
	fclose(fp);

	sprintf(buf, "sql -s %s < %s", G_getenv("DATABASE"), sqlFile);
	system(buf);
	unlink(sqlFile);
	exit(0);

/* Removed for a while
        
	ingresColumn(opt1->answer,flag->answer,sqlFile);

	unlink(sqlFile);
	exit(0);
*/

}

