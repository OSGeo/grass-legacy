#include "globals.h"

db_driver_execute_immediate (sql_statement)
    dbString *sql_statement;
{
    return execute_immediate (sql_statement);
}

execute_immediate (sql_statement)
    dbString *sql_statement;
{
    SQLTRCD rcd;
    char *s;

/* NOTE: this implementation can only execute sql with a database cursor
 * this means that SERVER specific sql (ie non-database requests can't
 * be processed
 */

    if (!check_for_open_database())
	return DB_FAILED;

    s = db_get_string (sql_statement);
    if (rcd = sqlcex (database_cursor, s, NULL_TERMINATED))
    {
	report_error_with_carot (database_cursor, rcd, s);
	return DB_FAILED;
    }
    return DB_OK;
}
