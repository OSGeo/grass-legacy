#include <dbmi.h>
#include "globals.h"

/* NAME: db_driver_create_table 
 * INPUT:
 * OUTPUT:
 * PROCESSING: issue a CREATE TABLE tableName (column data_type [ DEFAULT
 *   {NULL | CURRENT | USER} ] [ NOT NULL ], ... ) via EXECUTE IMMEDIATE
 */
db_driver_create_table (table)
dbTable *table;
{
  dbString cmd;
  $char *sqlcmd;
  dbColumn *col;
  dbValue *defaultValue;
  int i;

  db_init_string(&cmd);
  db_append_string(&cmd, "CREATE TABLE ");
  db_append_string(&cmd, db_get_table_name(table));
  db_append_string(&cmd, " (");
  for (i = 0; i < db_get_table_number_of_columns(table); i++) {
    if (i) db_append_string(&cmd,",");
    col = db_get_table_column(table, i);
    db_append_string(&cmd, db_get_column_name(col));
    db_append_string(&cmd, " ");
    build_column_type_name(col, &cmd);
    build_column_default(col, &cmd);
    if (!db_test_column_null_allowed(col)) {
      db_append_string(&cmd, " NOT NULL ");
    }
  }
  db_append_string(&cmd, ")");

  sqlcmd = db_get_string(&cmd);
  $EXECUTE IMMEDIATE $sqlcmd;
  db_free_string(&cmd);
  if (sql_error(NULL)) {
    return DB_FAILED;
  }
  return DB_OK;
}

build_column_type_name(col, cmd)
dbColumn *col;
dbString *cmd;
{
  int sql_type;
  char buf[50];

  sql_type = db_get_column_sqltype(col);
  db_append_string(cmd, db_sqltype_name(sql_type));

  /* append the (precision, scale) or (length) if necessary */
  switch (sql_type) {
    case DB_SQL_TYPE_CHARACTER:
	sprintf(buf, "(%d)", db_get_column_length(col));
	db_append_string(cmd, buf);
	break;
    case DB_SQL_TYPE_DECIMAL:
    case DB_SQL_TYPE_NUMERIC:
	sprintf(buf, "(%d,%d)", db_get_column_precision(col),
	db_get_column_scale(col));
	db_append_string(cmd, buf);
	break;
    default: break;
  }
}

build_column_default(col, cmd)
dbColumn *col;
dbString *cmd;
{
  dbString defaultString;

  if(db_test_column_has_default_value(col)) {
    db_append_string(cmd, " DEFAULT ");
    db_init_string(&defaultString);
    db_convert_column_default_value_to_string(col, &defaultString);
    if (db_get_column_sqltype(col) == DB_SQL_TYPE_CHARACTER)
      db_append_string(cmd, "\"");
    db_append_string(cmd, db_get_string(&defaultString));
    if (db_get_column_sqltype(col) == DB_SQL_TYPE_CHARACTER)
      db_append_string(cmd, "\"");
    db_free_string(&defaultString);
    db_append_string(cmd, " ");
  }
}

/* NAME: is_view
 * INPUT: name of the table (or view) to check
 * OUTPUT: boolean, true if name is a view, false otherwise
 * PROCESSING: query the SYSTABLES relation to get tabtype (table type)
 */
static int
is_view (name)
dbString *name;
{
  $char *tab_name;
  $string tab_type[2];
  $char tbname[50], tbcreator[50];

  tab_name = db_get_string(name);
  decompose_tablename(tab_name, tbcreator, tbname);

  $SELECT tabtype 
     INTO $tab_type 
     FROM systables 
    WHERE tabname = $tbname 
      AND owner = $tbcreator
      AND owner = USER;

  return *tab_type == 'V';
}

/* NAME: db_driver_drop_table 
 * INPUT: name of the table (or view) to be dropped
 * OUTPUT:
 * PROCESSING: issue a DROP TABLE name or DROP VIEW name via EXECUTE IMMEDIATE
 */
db_driver_drop_table (name)
dbString *name;
{
  $char cmd[200];

  if (is_view(name)) {
    sprintf(cmd, "DROP VIEW %s", db_get_string(name));
  }
  else {
    sprintf(cmd, "DROP TABLE %s", db_get_string(name));
  }
  $EXECUTE IMMEDIATE $cmd;
  if (sql_error(NULL))
    return DB_FAILED;
  return DB_OK;
}
