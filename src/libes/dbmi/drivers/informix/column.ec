#include <dbmi.h>

/* NAME: db_driver_add_column
 * INPUT: name of the table and a dbColumn describing what is to be added
 * OUTPUT: status
 * PROCESSING: ALTER TABLE table_name ADD column_name datatype [ DEFAULT [
 *   literal | NULL | CURRENT ] ] [ NOT NULL ]
 */
db_driver_add_column (tableName, column)
dbString *tableName;
dbColumn *column;
{
  dbString cmd;
  $char *sqlcmd;
  int status = DB_OK;

  db_init_string(&cmd);
  db_append_string(&cmd, "ALTER TABLE ");
  db_append_string(&cmd, db_get_string(tableName));
  db_append_string(&cmd, " ADD ");
  db_append_string(&cmd, db_get_column_name(column));
  db_append_string(&cmd, " ");
  build_column_type_name(column, &cmd);
  build_column_default(column, &cmd);
  if (!db_test_column_null_allowed(column))
    db_append_string(&cmd, " NOT NULL ");
  sqlcmd = db_get_string(&cmd);

  $EXECUTE IMMEDIATE $sqlcmd;

  if (sql_error(NULL)) status = DB_FAILED;
  db_free_string(&cmd);
  return status;
}

/* NAME: db_driver_drop_column
 * INPUT: the name of the table and the column to be removed from it
 * OUTPUT: status
 * PROCESSING: ALTER TABLE table_name DROP column_name
 */
db_driver_drop_column (tableName, columnName)
dbString *tableName, *columnName;
{
  $char cmd[100];

  sprintf(cmd, "ALTER TABLE %s DROP %s", db_get_string(tableName), 
    db_get_string(columnName));

  $EXECUTE IMMEDIATE $cmd;

  if (sql_error(NULL)) return DB_FAILED;
  return DB_OK;
}
