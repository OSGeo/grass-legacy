
#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;
#include "globals.h"

/* NAME: db_driver_insert
 * INPUT: a dbCursor which contains a cursor and a dbTable
 * OUTPUT: error code
 * PROCESSING: insert the information from the dbTable into the database using
 *  PUT
 */
db_driver_insert(dbc)
dbCursor *dbc;
{
  cursor *c;
  dbTable *table;
  $char *cname, *desc;

/* get the cursor */
  if (!(c = (cursor *) db_find_token(db_get_cursor_token(dbc)))) {
    db_error("cursor not found");
    return DB_FAILED;
  }
/* move the data from the dbTable to the descriptor */
  table = (dbTable *) db_get_cursor_table(dbc);
  desc = c->descriptor_name;
  if (move_data_to_descriptor(desc, table) == DB_FAILED) return DB_FAILED;

/* insert the information into the table */
  cname = c->cursor_name;
  $PUT $cname USING SQL DESCRIPTOR $desc;
  if (sql_error(NULL)) return DB_FAILED;
  return DB_OK;
}
