/****************************************************************************
 *
 * MODULE:       db.execute
 * AUTHOR(S):    Radim Blazek <radim.blazek gmail.com> (original contributor)
 *               Huidae Cho <grass4u gmail.com>, Glynn Clements <glynn gclements.plus.com>, Hamish Bowman <hamish_nospam yahoo.com>, Markus Neteler <neteler itc.it>, Stephan Holl
 * PURPOSE:      process one non-select sql statement
 * COPYRIGHT:    (C) 2002-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/dbmi.h>
#include <grass/codes.h>
#include <grass/glocale.h>

struct {
	char *driver, *database, *input;
	int i;
} parms;

void parse_command_line();
int get_stmt( FILE *fd, dbString *stmt);
int stmt_is_empty( dbString *stmt );
    
int
main( int argc, char *argv[] )
{
    dbString stmt;
    dbDriver *driver;
    dbHandle handle;
    int ret;
    FILE *fd;
    int error = 0;

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
    if (driver == NULL) {
	G_fatal_error(_("Unable to start driver <%s>"), parms.driver);
	exit(ERROR);
    }

    db_init_handle (&handle);
    db_set_handle (&handle, parms.database, NULL);
    if (db_open_database(driver, &handle) != DB_OK)
	exit(ERROR);

    while( get_stmt(fd, &stmt) )
    {
	if(!stmt_is_empty(&stmt)) {
	    G_debug (3, "sql: %s", db_get_string(&stmt) );
            ret = db_execute_immediate (driver, &stmt);
	    if ( ret != DB_OK ) {
	       if (parms.i){ /* ignore SQL errors */
		   G_warning(_("Error while executing: \"%s\""), db_get_string(&stmt));
		   error++;
	       }
	       else
	           G_fatal_error(_("Error while executing: \"%s\""), db_get_string(&stmt));
	    }
	}
    }

    db_close_database(driver);
    db_shutdown_driver(driver);

    exit(error);
}

void parse_command_line (int argc, char *argv[])
{
    struct Option *driver, *database, *input;
    struct Flag *i;
    struct GModule *module;
    char *drv, *db;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Set description */
    module              = G_define_module();
    module->keywords = _("database, SQL");
    module->description = _("Execute any SQL statement.");

    input 		= G_define_option();
    input->key 		= "input";
    input->key_desc 	= "filename";
    input->type 	= TYPE_STRING;
    input->required 	= NO;
    input->description 	= _("File containing SQL statements");
    input->gisprompt    = "old_file,file,input";

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->options     = db_list_drivers();
    driver->required 	= NO;
    driver->description = _("Driver name");
    if ( (drv=db_get_default_driver_name()) )
        driver->answer = drv;

    database 		= G_define_option();
    database->key 	= "database";
    database->type 	= TYPE_STRING;
    database->required 	= NO;
    database->description = _("Database name");
    if ( (db=db_get_default_database_name()) )
        database->answer = db;

    i = G_define_flag();
    i->key              = 'i';
    i->description      = _("Ignore SQL errors and continue");
    
    if(G_parser(argc, argv))
	exit(ERROR);

    parms.driver	= driver->answer;
    parms.database	= database->answer;
    parms.input		= input->answer;
    parms.i		= i->answer;
}


int get_stmt (FILE *fd, dbString *stmt)
{
    char buf[4000], buf2[4000];
    int len, row = 0;

    db_init_string (stmt);

    while ( fgets (buf, 4000, fd) != NULL ) {
        strcpy ( buf2, buf );
        G_chop (buf2);
        len = strlen (buf2);

	len = strlen (buf2);
	if ( buf2[ len - 1 ] == ';' ) {  /* end of statement */
	    buf2 [len - 1] = 0;          /* truncate ';' */
	    db_append_string (stmt, buf2); /* append truncated */
	    return 1;
	} else {
	    db_append_string (stmt, buf); /* append not truncated string (\n may be part of value) */
	}
	row++;
    }

    if ( row > 0 ) return 1;

    return 0;
}


int stmt_is_empty (dbString *stmt)
{
    char dummy[2];

    return (sscanf (db_get_string(stmt), "%1s", dummy) != 1);
}

