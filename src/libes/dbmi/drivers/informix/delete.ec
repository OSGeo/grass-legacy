#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>

$include sqltypes;
#include "globals.h"

/* NAME: db_driver_delete
 * INPUT: a dbCursor pointing to the record to be deleted
 * OUTPUT: an error code
 * PROCESSING: Delete the current record from the table
 */
db_driver_delete(dbc)
dbCursor *dbc;
{
  cursor *c;
  dbToken token;
  $char *delete_name;

/* get cursor token */
  token = db_get_cursor_token(dbc);

/* get the cursor by its token */
  if (!(c = (cursor *) db_find_token(token))) {
    db_error("cursor not found");
    return DB_FAILED;
  }

/* get delete statement name from cursor */
  delete_name = c->delete_name;

/* delete the record */
  /* The following does not work:  $DELETE FROM $tname WHERE CURRENT OF */
  /*  $cursor_name; as the delete syntax does not allow host variables to */
  /* contain the table and cursor names.  Stupid. */
  $EXECUTE $delete_name;

/* if error, return error */
  if (sql_error(NULL)) return DB_FAILED;

/* return okay */
  return DB_OK;
}
