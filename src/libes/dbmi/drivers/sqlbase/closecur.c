#include "globals.h"

int
db_driver_close_cursor (dbc)
    dbCursor *dbc;
{
    mycursor *c;

    c = (mycursor *) db_find_token (db_get_cursor_token(dbc));
    if (c == NULL)
	return DB_FAILED;
    
/* close cursor */
    close_mycursor (c);

    return DB_OK;
}
	
