/*
 **************************************************************
 * db.select -cdh driver=name database=name \
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
#include <stdlib.h>
#include "gis.h"
#include "dbmi.h"
#include "codes.h"
#include "glocale.h"

struct {
	char *driver, *database, *table, *sql, *fs, *vs, *nv, *input;
	int c,d,h;
} parms;

void parse_command_line();

int sel ( dbDriver *driver, dbString *stmt );
int get_stmt( FILE *fd, dbString *stmt );
int stmt_is_empty ( dbString *stmt );
void print_column_definition ( dbColumn *column );
    
int
main (int argc, char *argv[])
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
       
    db_init_string ( &stmt );
    
    driver = db_start_driver(parms.driver);
    if (driver == NULL)
	G_fatal_error(_("No db connection for driver <%s> defined. Run db.connect"), parms.driver);

    db_init_handle (&handle);
    db_set_handle (&handle, parms.database, NULL);
    if (db_open_database(driver, &handle) != DB_OK)
	exit(ERROR);

    if ( parms.sql ) {
	db_set_string ( &stmt, parms.sql );
	stat = sel(driver, &stmt);
    } else if ( parms.table ) {
	db_set_string ( &stmt, "select * from "); 
	db_append_string ( &stmt, parms.table); 
	stat = sel(driver, &stmt);
    } else { /* read stdin */
	stat = OK;
	while(stat == OK && get_stmt (fd, &stmt))
	{
	    if(!stmt_is_empty(&stmt))
		stat = sel(driver, &stmt);
	}
    }

    db_close_database(driver);
    db_shutdown_driver(driver);

    exit(stat);
}

int
sel (dbDriver *driver,
    dbString *stmt)
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
	    if (col) fprintf (stdout, "%s", parms.fs);
	    fprintf (stdout, "%s", db_get_column_name (column));
	}
	fprintf (stdout, "\n");
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
		fprintf (stdout, "%s%s", db_get_column_name (column), parms.fs);
	    if (col && parms.h)
		fprintf (stdout, "%s", parms.fs);
	    if(parms.nv && db_test_value_isnull(value))
		fprintf (stdout, "%s", parms.nv);
	    else
		fprintf (stdout, "%s", db_get_string (&value_string));
	    if (!parms.h)
		fprintf (stdout, "\n");
	}
	if (parms.h)
	    fprintf (stdout, "\n");
	else if (parms.vs)
	    fprintf (stdout, "%s\n", parms.vs);
    }

    return OK;
}

void
parse_command_line(int argc, char *argv[])
{
    struct Option *driver, *database, *table, *sql, *fs, *vs, *nv, *input;
    struct Flag *c,*d,*v;
    struct GModule *module;
    char *drv, *db;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    table 		= G_define_option();
    table->key 	        = "table";
    table->type 	= TYPE_STRING;
    table->required 	= NO;         
    table->description  = _("Table name, select all from this table");

    database 		= G_define_option();
    database->key 	= "database";
    database->type 	= TYPE_STRING;
    database->required 	= NO;        
    database->description = _("database name");
    if ( (db=db_get_default_database_name()) )
        database->answer = db;

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->options     = db_list_drivers();
    driver->required 	= NO;          
    driver->description = _("driver name");
    if ( (drv=db_get_default_driver_name()) )
        driver->answer = drv;

    sql 		= G_define_option();
    sql->key 	        = "sql";
    sql->type 	        = TYPE_STRING;
    sql->required 	= NO;         
    sql->description    = _("SQL select statement, for example: 'select * from rybniky where kapri = 'hodne'");

    fs 			= G_define_option();
    fs->key 		= "fs";
    fs->type 		= TYPE_STRING;
    fs->required 	= NO;
    fs->description 	= _("output field separator");
    fs->answer		= "|";

    vs 			= G_define_option();
    vs->key 		= "vs";
    vs->type 		= TYPE_STRING;
    vs->required 	= NO;
    vs->description 	= _("output vertical record separator");

    nv 			= G_define_option();
    nv->key 		= "nv";
    nv->type 		= TYPE_STRING;
    nv->required 	= NO;
    nv->description 	= _("null value indicator");

    input 		= G_define_option();
    input->key 		= "input";
    input->key_desc 	= "filename";
    input->type 	= TYPE_STRING;
    input->required 	= NO;
    input->description 	= _("filename with sql statement");
    input->gisprompt    = "file,file,file";

    c			= G_define_flag();
    c->key		= 'c';
    c->description	= _("do not include column names in output");

    d			= G_define_flag();
    d->key		= 'd';
    d->description	= _("describe query only (don't run it)");

    v			= G_define_flag();
    v->key		= 'v';
    v->description	= _("vertical output (instead of horizontal)");

    /* Set description */
    module              = G_define_module();
    module->description = _("Select data from database.");


    if(G_parser(argc, argv))
	exit(ERROR);

    parms.driver	= driver->answer;
    parms.database	= database->answer;
    parms.table 	= table->answer;
    parms.sql  	        = sql->answer;
    parms.fs		= fs->answer;
    parms.vs		= vs->answer;
    parms.nv		= nv->answer;
    parms.input		= input->answer;
    if ( !c->answer )  parms.c = 1; else  parms.c = 0;
    parms.d		= d->answer;
    if ( !v->answer )  parms.h = 1; else  parms.h = 0;

    if (!parms.fs) parms.fs = "";
    if (parms.input && *parms.input == 0)
    {
	G_usage();
	exit(1);
    }
}

int
get_stmt(FILE *fd,
    dbString *stmt)
{
    char buf[1024];
    int n;
    static int first = 1;

    db_zero_string (stmt);

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

int
stmt_is_empty(dbString *stmt)
{
    char dummy[2];

    return (sscanf (db_get_string(stmt), "%1s", dummy) != 1);
}

void
print_column_definition(dbColumn *column)
{
    G_message (_("column%s%s\n"), parms.fs, db_get_column_name(column));
    G_message (_("type%s%s\n"), parms.fs, db_sqltype_name(db_get_column_sqltype(column)));
    G_message (_("len%s%d\n"), parms.fs, db_get_column_length(column));
    G_message (_("scale%s%d\n"), parms.fs, db_get_column_scale(column));
    G_message (_("precision%s%d\n"), parms.fs, db_get_column_precision(column));
    if (parms.vs)
	fprintf (stdout, "%s\n", parms.vs);
}
