/*
 *   g.column.pg
 *
 *   Generate list of database columns for a specified table
 *   in the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/26/91
 */

/*  modifcation 11/98
 *      to connect to Postgress v6.4
 *      to use libpq
 *  Carl Anderson 11/13/98
*/

#include "gis.h"
#include <stdio.h>
#include <stdlib.h>
#include "glocale.h"

#define MAIN
int pgColumn(char *);

int main(argc, argv)
     int argc;
     char **argv;
{
    char *dbname;
    struct Flag *flag;
    struct Option *tbl;
    int stat;
    static char SQL[1024];


    /* Initialize the GIS calls */
    G_gisinit(argv[0]);

    tbl = G_define_option();
    tbl->key = "table";
    tbl->type = TYPE_STRING;
    tbl->required = YES;
    tbl->multiple = NO;
    tbl->description = _("The table name in the selected database:");

    flag = G_define_flag();
    flag->key = 'v';
    flag->description = _("Use flag -v for full output.");

    /* Check DATABASE env variable */
    if ((dbname = G__getenv("PG_DBASE")) == NULL) {
	fprintf(stderr,
		_
		("Please run g.select.pg to identify a current database.\n"));
	exit(-1);
    }

    /* Invoke parser */
    if (G_parser(argc, argv)) {
	exit(-1);
    }

    if (flag->answer) {
	sprintf(SQL, "SELECT a.attname as ColumnName, t.typname as Type,
		  a.attlen as Length,
		  a.atttypmod as \"Mod_Length\"
		 FROM pg_class c, pg_attribute a, pg_type t
		 WHERE  c.relname = '%s' 
		  and a.attnum > 0
		  and a.attrelid = c.oid
		  and a.atttypid = t.oid
		  order by attnum", tbl->answer);

    }
    else {
	/* no column descriptions */
	sprintf(SQL, "SELECT a.attname as ColumnName
		 FROM pg_class c, pg_attribute a
		 WHERE  c.relname = '%s' 
		  and a.attnum > 0
		  and a.attrelid = c.oid
		  order by attnum", tbl->answer);
    }



    stat = pgColumn(SQL);

    return (stat);

}
