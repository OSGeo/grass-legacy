/**************************************************************
 * db.columns driver=name database=name table=name
 *
 *
 *   list the column names for a table
 ****************************************************************/

#include "gis.h"
#include "dbmi.h"
#include "codes.h"
#include <stdlib.h>
#include "glocale.h"

struct {
	char *driver, *database, *table;
} parms;

void parse_command_line();

int
main(int argc, char *argv[])
{
    dbDriver *driver;
    dbHandle handle;
    dbTable *table;
    dbString table_name;
    int col, ncols;

    parse_command_line (argc, argv);

    driver = db_start_driver(parms.driver);
    if (driver == NULL)
        G_fatal_error(_("No db connection for driver <%s> defined. Run db.connect"), parms.driver);
       
    db_init_handle (&handle);
    db_set_handle (&handle, parms.database, NULL);
    if (db_open_database(driver, &handle) != DB_OK)
    {
	exit(ERROR);
    }
    db_init_string(&table_name);
    db_set_string(&table_name, parms.table);
    if(db_describe_table (driver, &table_name, &table) != DB_OK)
	exit(ERROR);

    db_close_database(driver);
    db_shutdown_driver(driver);

    ncols = db_get_table_number_of_columns(table);
    for (col = 0; col < ncols; col++)
	G_message ("%s\n", db_get_column_name(db_get_table_column(table, col)));
    exit(OK);
}

void
parse_command_line(int argc, char *argv[])
{
    struct Option *driver, *database, *table;
    struct GModule *module;
    char *drv, *db;

    /* Initialize the GIS calls */
        G_gisinit(argv[0]) ;

    table 		= G_define_option();
    table->key 		= "table";
    table->type 	= TYPE_STRING;
    table->required 	= YES;
    table->description 	= _("table name");

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->options     = db_list_drivers();
    driver->required 	= NO;
    driver->description = _("driver name");
    if ( (drv=db_get_default_driver_name()) )
        driver->answer = drv;

    database 		= G_define_option();
    database->key 	= "database";
    database->type 	= TYPE_STRING;
    database->required 	= NO;
    database->description = _("database name");
    if ( (db=db_get_default_database_name()) )
         database->answer = db;

    /* Set description */
    module              = G_define_module();
    module->description = _("list all columns for a given table.");

        
    if(G_parser(argc, argv))
	exit(ERROR);

    parms.driver	= driver->answer;
    parms.database	= database->answer;
    parms.table		= table->answer;
}
