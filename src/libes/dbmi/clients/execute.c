/**************************************************************
 * db.execute driver=name database=name [location=name] [input=filename]
 *
 *
 *   process one non-select sql statement.
 *   errors cause an error message to be printed to stderr and exit(1)
 *   successful execution results in exit(0)
 ****************************************************************/

#include "gis.h"
#include "dbmi.h"
#include "codes.h"

struct {
	char *driver, *database, *location, *input;
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
	    stat = execute_immediate(driver, &stmt);
    }

    db_close_database(driver);
    db_shutdown_driver(driver);

    exit(stat);
}

void
parse_command_line(argc, argv) char *argv[];
{
    struct Option *driver, *database, *location, *input;

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->required 	= NO;
    driver->description = "driver name";

    database 		= G_define_option();
    database->key 	= "database";
    database->type 	= TYPE_STRING;
    database->required 	= NO;
    database->description = "database name";

    location 		= G_define_option();
    location->key 	= "location";
    location->type 	= TYPE_STRING;
    location->required 	= NO;
    location->description = "database location";

    input 		= G_define_option();
    input->key 		= "input";
    input->key_desc 	= "filename";
    input->type 	= TYPE_STRING;
    input->required 	= NO;
    input->description 	= "filename with sql statement";

    G_disable_interactive();
    
    if (argc > 1) {
	if(G_parser(argc, argv)) exit(ERROR);
    }

    parms.driver	= driver->answer;
    parms.database	= database->answer;
    parms.location	= location->answer;
    parms.input		= input->answer;
}

execute_immediate (driver, stmt)
    dbDriver *driver;
    dbString *stmt;
{
    return db_execute_immediate (driver, stmt) == DB_OK ? OK : ERROR;
}

get_stmt(fd, stmt)
    FILE *fd;
    dbString *stmt;
{
    char buf[1024];
    int n;
    static int first = 1;

    db_init_string (stmt);

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
