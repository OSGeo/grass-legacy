#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include "globals.h"

$include sqltypes;

/* NAME: do_close_cursor
 * INPUT: dbMessage
 * OUTPUT: error code
 * PROCESSING: CLOSE cursor; FREE cursor
 */
int
db_driver_close_cursor(dbc)
   dbCursor *dbc;
{
  cursor *c;
  $char *cursor_name;
  $char *descriptor_name;
  $char *statement_name;
  $char *update_name;
  $char *delete_name;

/* get my cursor via the dbc token */
  c = (cursor *) db_find_token(db_get_cursor_token(dbc));
  if (c == NULL)
    return DB_FAILED;

/* get cursor name */
  cursor_name = c->cursor_name;

/* CLOSE cursorname */
  $CLOSE $cursor_name;
  if (sql_error(NULL)) {
    return DB_FAILED;
  }

/* FREE cursorname */
  $FREE $cursor_name;
  if (sql_error(NULL)) {
    return DB_FAILED;
  }

/* FREE statement_name */
  statement_name = c->statement_name;
  $FREE $statement_name;
  if (sql_error(NULL)) {
    return DB_FAILED;
  }

/* DEALLOCATE descriptor */
  descriptor_name = c->descriptor_name;
  $DEALLOCATE DESCRIPTOR $descriptor_name;

/* if this cursor was opened for update, free the update and delete */
/* statements */
  if (db_test_cursor_type_update(dbc)) {
    update_name = c->update_name;
    $FREE $update_name;
    delete_name = c->delete_name;
    $FREE $delete_name;
  }

/* free_cursor(cursor) */
  free_cursor(c);
  if (sql_error(NULL)) {
    return DB_FAILED;
  }

  return DB_OK;
}
