#include "globals.h"

int
db_driver_open_select_cursor(select_string, dbc, mode)
    dbString *select_string;
    dbCursor *dbc;
    int mode;
{
    mycursor *c;

/* allocate cursor */
    c = open_mycursor();
    if (c == NULL)
	return DB_FAILED;

    db_set_cursor_mode(dbc,mode);
    db_set_cursor_type_readonly(dbc);

    if(compile_cursor(c, select_string, dbc) != DB_OK) {
	close_mycursor(c);
	return DB_FAILED;
    }

/* set dbCursor's token for my cursor */
    db_set_cursor_token(dbc, c->token);



    /* return OK */
    return DB_OK;
}
