#include "globals.h"

int
db_driver_delete (dbc)
    dbCursor *dbc;
{
    mycursor *c;
    char cmd[1024];
    SQLTRCD rcd;	/*return code */

    c = (mycursor *) db_find_token (db_get_cursor_token (dbc));
    if (c == NULL)
    {
	db_error ("invalid cursor");
	return DB_FAILED;
    }

    sprintf (cmd, "DELETE FROM %s WHERE CURRENT OF %s",
	db_get_string (&c->tableName), c->name);

/* compile the DELETE */
    rcd = sqlcom (database_cursor, cmd, NULL_TERMINATED);
    if(rcd)
    {
	report_error_with_carot (database_cursor, rcd, cmd);
	return DB_FAILED;
    }
/* execute the DELETE */
    rcd = sqlexe (database_cursor);
    if(rcd)
    {
	report_error (rcd, "sqlexe() failed");
	return DB_FAILED;
    }

    /* return OK */
    return DB_OK;
}
