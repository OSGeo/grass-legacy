/*
 * $Id$
 *
 **************************************************************
 * db.select -cdh driver=name database=name [location=name] \
 *	      [fs=|] [vs=] [nv=null-indicator] [input=filename]
 *
 *
 *   process one sql select statement.
 *   errors cause an error message to be printed to stderr and exit(1)
 *   successful execution results in exit(0)
 *
 *   fs = output field separator
 *   vs = output vertical record separator
 *   nv = value to represent null values
 *   -c = output the column names
 *   -d = describe query only, don't run it
 *   -h = use horizontal output format instead of vertical
 ****************************************************************/

#include "gis.h"
#include "dbmi.h"
#include "codes.h"

struct {
	char *driver, *database, *location, *fs, *vs, *nv, *input;
	int c,d,h;
} parms;

void parse_command_line();

main(argc, argv) char *argv[];
{
    dbString stmt;
    dbDriver *driver;
    dbHandle handle;
    int stat;
    FILE *fd;

    parse_command_line (argc, argv);

    if (parms.input)
    {
	fd = fopen (parms.input, "r");
	if (fd == NULL)
	{
	    perror (parms.input);
	    exit(ERROR);
	}
    }
    else
	fd = stdin;

    driver = db_start_driver(parms.driver);
    if (driver == NULL)
	exit(ERROR);

    db_init_handle (&handle);
    db_set_handle (&handle, parms.database, parms.location);
    if (db_open_database(driver, &handle) != DB_OK)
	exit(ERROR);


    stat = OK;
    while(stat == OK && get_stmt (fd, &stmt))
    {
	if(!stmt_is_empty(&stmt))
	    stat = select(driver, &stmt);
    }

    db_close_database(driver);
    db_shutdown_driver(driver);

    exit(stat);
}

select (driver, stmt)
    dbDriver *driver;
    dbString *stmt;
{
    dbCursor cursor;
    dbTable *table;
    dbColumn *column;
    dbValue *value;
    dbString value_string;
    int col, ncols;
    int more;

    if (db_open_select_cursor(driver, stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
	return ERROR;
    table = db_get_cursor_table (&cursor);
    ncols = db_get_table_number_of_columns (table);
    if(parms.d)
    {
	for(col = 0; col < ncols; col++)
	{
	    column = db_get_table_column(table, col);
	    print_column_definition(column);
	}
	return OK;
    }

    db_init_string (&value_string);

/* column names if horizontal output */
    if (parms.h && parms.c)
    {
	for (col = 0; col < ncols; col++)
	{
	    column = db_get_table_column(table, col);
	    if (col) fprintf (stdout,"%s", parms.fs);
	    fprintf (stdout,"%s", db_get_column_name (column));
	}
	fprintf (stdout,"\n");
    }

/* fetch the data */
    while(1)
    {
	if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    return ERROR;
	if (!more)
	    break;

	for (col = 0; col < ncols; col++)
	{
	    column = db_get_table_column(table, col);
	    value  = db_get_column_value(column);
	    db_convert_column_value_to_string (column, &value_string);
	    if (parms.c && !parms.h)
		fprintf (stdout,"%s%s", db_get_column_name (column), parms.fs);
	    if (col && parms.h)
		fprintf (stdout,"%s", parms.fs);
	    if(parms.nv && db_test_value_isnull(value))
		fprintf (stdout,"%s", parms.nv);
	    else
		fprintf (stdout,"%s", db_get_string (&value_string));
	    if (!parms.h)
		fprintf (stdout,"\n");
	}
	if (parms.h)
	    fprintf (stdout,"\n");
	else if (parms.vs)
	    fprintf (stdout,"%s\n", parms.vs);
    }

    return OK;
}

void
parse_command_line(argc, argv) char *argv[];
{
    struct Option *driver, *database, *location, *fs, *vs, *nv, *input;
    struct Flag *c,*d,*h;

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->required 	= NO;           /* changed to NO, RB 4/2000 */
    driver->description = "driver name";

    database 		= G_define_option();
    database->key 	= "database";
    database->type 	= TYPE_STRING;
    database->required 	= NO;         /* changed to NO, RB 4/2000 */
    database->description = "database name";

    location 		= G_define_option();
    location->key 	= "location";
    location->type 	= TYPE_STRING;
    location->required 	= NO;
    location->description = "database location";

    fs 			= G_define_option();
    fs->key 		= "fs";
    fs->type 		= TYPE_STRING;
    fs->required 	= NO;
    fs->description 	= "output field separator";
    fs->answer		= "|";

    vs 			= G_define_option();
    vs->key 		= "vs";
    vs->type 		= TYPE_STRING;
    vs->required 	= NO;
    vs->description 	= "output vertical record separator";

    nv 			= G_define_option();
    nv->key 		= "nv";
    nv->type 		= TYPE_STRING;
    nv->required 	= NO;
    nv->description 	= "null value indicator";

    input 		= G_define_option();
    input->key 		= "input";
    input->key_desc 	= "filename";
    input->type 	= TYPE_STRING;
    input->required 	= NO;
    input->description 	= "filename with sql statement";

    c			= G_define_flag();
    c->key		= 'c';
    c->description	= "include column names in output";

    d			= G_define_flag();
    d->key		= 'd';
    d->description	= "describe query only (don't run it)";

    h			= G_define_flag();
    h->key		= 'h';
    h->description	= "horizontal output (instead of vertical)";

    G_disable_interactive();

    if (argc > 1) {
	if(G_parser(argc, argv)) exit(ERROR);
    }

    parms.driver	= driver->answer;
    parms.database	= database->answer;
    parms.location	= location->answer;
    parms.fs		= fs->answer;
    parms.vs		= vs->answer;
    parms.nv		= nv->answer;
    parms.input		= input->answer;
    parms.c		= c->answer;
    parms.d		= d->answer;
    parms.h		= h->answer;

    if (!parms.fs) parms.fs = "";
    if (parms.input && *parms.input == 0)
    {
	G_usage();
	exit(1);
    }
}

get_stmt(fd, stmt)
    FILE *fd;
    dbString *stmt;
{
    char buf[1024];
    int n;
    static int first = 1;

    db_init_string (stmt);

/* this is until get_stmt is smart enough to handle multiple stmts */
    if (!first)
	return 0;
    first = 0;

    while ( ( n = fread (buf, 1, sizeof(buf)-1, fd)) > 0)
    {
	buf[n] = 0;
	db_append_string (stmt, buf);
    }

    return 1;
}

stmt_is_empty(stmt)
    dbString *stmt;
{
    char dummy[2];

    return (sscanf (db_get_string(stmt), "%1s", dummy) != 1);
}

print_column_definition(column)
    dbColumn *column;
{
    fprintf (stdout,"column%s%s\n", parms.fs, db_get_column_name(column));
    fprintf (stdout,"type%s%s\n", parms.fs, db_sqltype_name(db_get_column_sqltype(column)));
    fprintf (stdout,"len%s%d\n", parms.fs, db_get_column_length(column));
    fprintf (stdout,"scale%s%d\n", parms.fs, db_get_column_scale(column));
    fprintf (stdout,"precision%s%d\n", parms.fs, db_get_column_precision(column));
    if (parms.vs)
	fprintf (stdout,"%s\n", parms.vs);
}
