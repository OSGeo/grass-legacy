#include <stdio.h>
#include <dbmi.h>
#include "globals.h"

/* NAME: db_driver_open_select
 * INPUT: select string to be opened, cursor to be opened
 * OUTPUT: return code and opened cursor
 * PROCESSING: build a select statement, PREPARE it, DECLARE a cursor for it, 
 *  and OPEN the cursor
 */
int
db_driver_open_select_cursor(select_string, dbc, mode)
dbCursor *dbc;
dbString *select_string;
int mode;
{
  cursor *c;

/* allocate cursor */
  if (!(c = make_cursor())) {
    return DB_FAILED;
  }

/* ready cursor */
  db_set_cursor_mode(dbc,mode);
  db_set_cursor_type_readonly(dbc);
  if(ready_cursor(c, select_string, dbc) != DB_OK) {
    free_cursor(c);
    return DB_FAILED;
  }
/* set dbCursor's token for my cursor */
  db_set_cursor_token(dbc, c->token);

/* return OK */
  return DB_OK;
}
