
#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>
$include sqltypes;
#include "globals.h"

/* NAME: db_driver_open_insert_cursor
 * INPUT: table to be opened, cursor to be opened
 * OUTPUT: return code and opened cursor
 * PROCESSING: build an INSERT statement for INSERT, PREPARE it 
 */
int
db_driver_open_insert_cursor(dbc)
dbCursor *dbc;
{
  dbString buffer, s;
  dbTable *table;
  int numColsNonDefault = 0;
  cursor *c;

  table = db_get_cursor_table(dbc);
/* allocate cursor */
  if (!(c = make_cursor())) {
    return DB_FAILED;
  }
/* build an insert statement header into buffer */
  db_init_string(&buffer);
  db_append_string(&buffer, "INSERT INTO ");

/* add table name to buffer */
  db_append_string(&buffer, db_get_table_name(table));

/* append "(" to buffer */
  db_append_string(&buffer, " ( ");

/* append column list to buffer */
  db_init_string(&s);
  build_column_list_from_cursor(dbc, &s, &numColsNonDefault);
  db_append_string(&buffer, db_get_string(&s));
  db_free_string(&s);

/* append ")" to buffer */
  db_append_string(&buffer, " ) ");

/* append "VALUES" to buffer */
  db_append_string(&buffer, "VALUES ");

/* append "(" to buffer */
  db_append_string(&buffer, " ( ");

/* append "?" list to buffer */
  build_question_list(&s, numColsNonDefault);
  db_append_string(&buffer, db_get_string(&s));
  db_free_string(&s);

/* append ")" to buffer */
  db_append_string(&buffer, " ) ");

/* ready cursor */
  db_unset_cursor_mode_scroll(dbc);  /* scroll doesn't make sense */
  db_unset_cursor_mode_insensitive(dbc);  /* insensitive doesn't make sense */
  db_set_cursor_type_insert(dbc);
  if(ready_cursor(c, &buffer, dbc) != DB_OK) {
    free_cursor(c);
    db_free_string(&buffer);
    return DB_FAILED;
  }
  db_free_string(&buffer);
  db_set_cursor_token(dbc, c->token);
  return DB_OK;
}

build_column_list_from_cursor(dbc, string, numColsNotIncluded)
dbCursor *dbc;
dbString *string;
int *numColsNotIncluded;
{
  dbTable *table;
  $int colCount;
  $char *colName;
  dbColumn *column;
  int col;

  table = db_get_cursor_table(dbc);
  *numColsNotIncluded = 0;
  colCount = db_get_table_number_of_columns(table);
  for (col = 0; col < colCount; col++) {
    column = db_get_table_column(table, col);
    if (!(db_test_column_use_default_value(column)
	  || (db_test_cursor_type_update(dbc)
	      && db_get_column_sqltype(column) == DB_SQL_TYPE_SERIAL))
	  || (db_test_cursor_column_for_update(dbc, column))) {
      colName = db_get_column_name(column);
      if ((*numColsNotIncluded)++) db_append_string(string, ", ");
      db_append_string(string, colName);
    }
  }
}

build_question_list(string, numQuestions)
dbString *string;
int numQuestions;
{
  int i;
  for (i = 0; i < numQuestions; i++) {
    if (i != 0) db_append_string(string, ", ");
    db_append_string(string, "?");
  }
}

ready_cursor(c, declare_string, dbc)
cursor *c;
dbString *declare_string;
dbCursor *dbc;
{
  $char *sname, *dstring, *cname, *dname;
  dbTable *table;

/* PREPARE buffer */
  sname = c->statement_name;
  dstring = db_get_string(declare_string);
  $PREPARE $sname FROM $dstring;

/* if error, free cursor and return error code */
  if(sql_error(NULL)) {
    return DB_FAILED;
  }

/* DECLARE cursor */
  cname = c->cursor_name;
  /* attempt to declare for scroll mode; if it is not supported, declare */
  /* without and flag the client */
  if (db_test_cursor_mode_scroll(dbc)) {
    $DECLARE $cname SCROLL CURSOR FOR $sname;
    if (sql_error(NULL)) {
      return DB_FAILED;
    }

    /* OPEN cursor */
    $OPEN $cname;

    if(sql_error(NULL)) {
      db_unset_cursor_mode_scroll(dbc);
      $DECLARE $cname CURSOR FOR $sname;
      $OPEN $cname;
    }
  }
  else
    $DECLARE $cname CURSOR FOR $sname;
    $OPEN $cname;

/* if error, free cursor, free statement, and return error code */
  if(sql_error(NULL)) {
    $FREE $sname;
    return DB_FAILED;
  }

/* DESCRIBE statement */
  dname = c->descriptor_name;
  $ALLOCATE DESCRIPTOR $dname;

/* if error, free cursor, free statement, and return error */
  if(sql_error(NULL)) {
    $FREE $sname;
    $FREE $cname;
    return DB_FAILED;
  }

  $DESCRIBE $sname USING SQL DESCRIPTOR $dname;

/* if error, free cursor, free statement, free descriptor, and return error */
  if(sql_error(NULL)) {
    $FREE $sname;
    $FREE $cname;
    $DEALLOCATE DESCRIPTOR $dname;
    return DB_FAILED;
  }

  if (alloc_descriptor_data(dname, &table) != DB_OK) {
    $FREE $sname;
    $FREE $cname;
    $DEALLOCATE DESCRIPTOR $dname;
    db_free_table(table);
    return DB_FAILED;
  }
  db_set_cursor_table(dbc, table);

/* recored table with dbCursor */
  db_set_cursor_table(dbc, table);

/* record cursor with dbCursor */
  db_set_cursor_token(dbc, db_new_token(c));

/* return OK */
  return DB_OK;
}
