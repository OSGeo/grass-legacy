#include "dbmi.h"
#include "globals.h"

db_driver_execute_immediate (sql_statement)
    dbString *sql_statement;
{
  $char *s;

  s = db_get_string (sql_statement);
  $EXECUTE IMMEDIATE $s;
  if (sql_error(NULL))
    return DB_FAILED;
  return DB_OK;
}
