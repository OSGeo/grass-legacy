#include "globals.h"

db_driver_init (argc, argv) char *argv[];
{
    debug_fd = fopen ("/dev/tty","w");
    if (debug_fd == NULL)
	debug_fd = fopen ("/dev/null","w");

/* the SQLBASE C/API routines sometimes write to stderr
 * turn this off by turning off stderr
 */
#ifdef __CODECENTER__
    db_auto_print_errors(1);
#else
    if(!getenv("SQLBASE_DRIVER_STDERR"))
    {
	freopen ("/dev/null", "w", stderr);
    }
#endif

/* set the database cursor to 'not open' */
    database_cursor = 0;
    database_name   = NULL;

/* set the server handle to 'not connected' */
    server_handle = 0;
    server_name   = NULL;

/* change into the directory with the SQL.INI file */
/*
    if(chdir_sqlbase() != DB_OK)
	return DB_FAILED;
*/

    return DB_OK;
}
