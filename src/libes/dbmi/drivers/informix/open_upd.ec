#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;

#include "globals.h"

/* NAME: db_driver_open_update_cursor
 * INPUT: table to be opened, cursor to be opened
 * OUTPUT: return code and opened cursor
 * PROCESSING: build a select statement for UPDATE, PREPARE it, DECLARE a 
 *  cursor for it, and OPEN the cursor
 */
int
db_driver_open_update_cursor(table_name, select_string, dbc, mode)
dbString *table_name;
dbString *select_string;
dbCursor *dbc;
int mode;
{
  cursor *c;

/* allocate cursor */
  if (!(c = make_cursor())) {
    return DB_FAILED;
  }
  if (strlen(db_get_string(table_name)) > sizeof(c->update_table_name) - 1) {
    db_error("table name too long");
    return DB_FAILED;
  }
  strcpy(c->update_table_name, db_get_string(table_name));
  db_set_cursor_type_update(dbc);

/* append "FOR UPDATE" onto the end of the select_string */
  db_append_string(select_string, " FOR UPDATE");
/* ready cursor */
  if (ready_cursor(c, select_string, dbc) != DB_OK) {
    free_cursor(c);
    return DB_FAILED;
  }

  db_set_cursor_token(dbc, c->token);
  return DB_OK;
}

int
db_driver_bind_update(dbc)
dbCursor *dbc;
{
  $char *uname, *ustring, *dname, *udname, *delete_name;
  $int numColsToUpdate, selCount, i, ucol, type, len;
  dbString u;
  cursor *c;
  int numSerial;

  if (!(c = (cursor *) db_find_token(db_get_cursor_token(dbc)))) {
    db_error("cursor not found");
    return DB_FAILED;
  }
  dname = c->descriptor_name;

/* build "UPDATE tablename SET ( colname, ...) = (?,?...) WHERE CURRENT" */
/*  "OF cursorName" */
  db_init_string(&u);
  db_append_string(&u, "UPDATE ");
  db_append_string(&u, c->update_table_name);
  db_append_string(&u, " SET ( ");
  build_column_list_from_cursor(dbc, &u, &numColsToUpdate);
  db_append_string(&u, " ) = ( ");
  build_question_list(&u, numColsToUpdate);
  db_append_string(&u, " ) WHERE CURRENT OF ");
  db_append_string(&u, c->cursor_name);

/* PREPARE update sting */
  uname = c->update_name;
  ustring = db_get_string(&u);
  $PREPARE $uname FROM $ustring;

/* if error, free cursor, free statement, and return error code */
  if (sql_error(NULL)) goto error1;

/* allocate the descriptor for update */
  udname = c->update_descriptor_name;
  $ALLOCATE DESCRIPTOR $udname WITH MAX $numColsToUpdate;

/* if error, free cursor, free statement, and return error code */
  if (sql_error(NULL)) goto error1;

/* effectively do a describe on the update statement */
  $GET DESCRIPTOR $dname $selCount = COUNT;
  $SET DESCRIPTOR $udname COUNT = $numColsToUpdate;
  numSerial = 0;
  for (i = 1; i <= selCount; i++) {
    ucol = i - numSerial;
    $GET DESCRIPTOR $dname VALUE $i $type = TYPE, $len = LENGTH;
    if (type == SQLSERIAL) { numSerial++; continue;}
    len++; /* don't know why, but if left out, data gets last char removed */
    $SET DESCRIPTOR $udname VALUE $ucol TYPE = $type, LENGTH = $len;
  }

/* if error, free cursor, free statement, and return error code */
  if (sql_error(NULL)) goto error1;

/* build delete statement */
  db_set_string(&u, "");
  db_append_string(&u, "DELETE FROM ");
  db_append_string(&u, c->update_table_name);
  db_append_string(&u, " WHERE CURRENT OF ");
  db_append_string(&u, c->cursor_name);

/* PREPARE delete string */
  delete_name = c->delete_name;
  ustring = db_get_string(&u);
  $PREPARE $delete_name FROM $ustring;

/* if error, free cursor, free statement, and return error code */
  if (sql_error(NULL)) goto error1;

/* record cursor with dbCursor */
  db_set_cursor_token(dbc, c->token);
  db_free_string(&u);
  return DB_OK;

error1: 
  db_free_string(&u);
  free_cursor(c);
  return DB_FAILED;
}
