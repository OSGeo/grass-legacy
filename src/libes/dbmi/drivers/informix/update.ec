
#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;
#include "globals.h"

/* NAME: db_driver_update
 * INPUT: dbCursor
 * OUTPUT: error code
 * PROCESSING: update the table using the data contained in the dbTable in the
 *  dbCursor, using EXECUTE USING SQL DESCRIPTOR
 */
db_driver_update(dbc)
dbCursor *dbc;
{
  cursor *c;
  $char *uname, *dname;
  dbTable *table;

  if (!(c = (cursor *) db_find_token(db_get_cursor_token(dbc)))) {
    db_error("cursor not found");
    return DB_FAILED;
  }

  uname = c->update_name;
  dname = c->update_descriptor_name;
  table = db_get_cursor_table(dbc);
  if(move_data_to_descriptor(dname, table) == DB_FAILED) {
    return DB_FAILED;
  }
  $EXECUTE $uname USING SQL DESCRIPTOR $dname;
  if (sql_error(NULL)) return DB_FAILED;
  return DB_OK;
}

/* NAME: move_data_to_descriptor
 * INPUT: descriptor to move data to, table to move data from
 * OUTPUT: status code
 * PROCESSING: for each column in the table, move the data to the descriptor,
 *   doing appropriate type conversions
 */
move_data_to_descriptor(desc, table)
$parameter char *desc;
dbTable *table;
{
  $int colCount, dcol;
  int col, numSerial;
  dbColumn *colptr;
  dbValue *value;
  int sqltype;
  $int i;
  $char char_value[MAX_STRING_SIZE];
  $double d;
  short dvec[3];
  $long date_value, current_date_value;
  $datetime year to fraction(5) datetime_value, current_datetime_value;
  $interval year to month ym_interval_value;
  $interval day to fraction(5) df_interval_value;
  char temp[100];

  rtoday(&current_date_value);
  dtcurrent(&current_datetime_value);
  
  colCount = db_get_table_number_of_columns(table);
  numSerial = 0;
  for (col = 0; col < colCount; col++) {
    colptr = db_get_table_column(table, col);
    if (db_get_column_host_type(colptr) == SQLSERIAL) {
      numSerial++;
      continue;
    }
    dcol = col + 1 - numSerial;
    sqltype = db_get_column_sqltype(colptr);
    value = db_get_column_value(colptr);

    /* if the value is NULL, set INDICATOR to -1 and continue to next column */
    if (db_test_value_isnull(value)) {
      $SET DESCRIPTOR $desc VALUE $dcol INDICATOR = -1;
      continue;
    }

    /* value is not NULL, so set indicator to 0. This requires that DATA be */
    /* set by the time the descriptor is given to Informix */
    $SET DESCRIPTOR $desc VALUE $dcol INDICATOR = 0;

    switch (db_sqltype_to_Ctype(sqltype)) {
      case DB_C_TYPE_STRING:
	strncpy(char_value, db_get_value_string(value), sizeof(char_value)-1);
	char_value[sizeof(char_value)-1] = '\0';
        $SET DESCRIPTOR $desc VALUE $dcol DATA = $char_value;
	break;

      case DB_C_TYPE_INT:
	i = db_get_value_int(value);
        $SET DESCRIPTOR $desc VALUE $dcol DATA = $i;
	break;

      case DB_C_TYPE_DOUBLE:
	d = db_get_value_double(value);
        $SET DESCRIPTOR $desc VALUE $dcol DATA = $d;
	break;

      case DB_C_TYPE_DATETIME:
	switch (sqltype  & ~DB_DATETIME_MASK) {
	  case DB_SQL_TYPE_DATE:
            if (db_test_value_datetime_current(value)) {
              $SET DESCRIPTOR $desc VALUE $dcol DATA = $current_date_value;
              break;
            }
            dvec[2] = db_get_value_year(value);
            dvec[0] = db_get_value_month(value);
            dvec[1] = db_get_value_day(value);
            if (rmdyjul(dvec, &date_value) != 0) {
	      db_error("DATE conversion failed");
	      return DB_FAILED;
	    }
            $SET DESCRIPTOR $desc VALUE $dcol DATA = $date_value;
	    break;
	  case DB_SQL_TYPE_TIMESTAMP:
            if (db_test_value_datetime_current(value)) {
              $SET DESCRIPTOR $desc VALUE $dcol DATA = $current_datetime_value;
              break;
            }
	    sprintf(temp, "%d-%d-%d %d:%d:%08.5f", db_get_value_year(value),
	      db_get_value_month(value), db_get_value_day(value),
	      db_get_value_hour(value), db_get_value_minute(value),
	      db_get_value_seconds(value));
	    if (dtcvasc(temp, &datetime_value) !=0) {
	      db_error("datetime conversion failed");
	      return DB_FAILED;
	    }
            $SET DESCRIPTOR $desc VALUE $dcol DATA = $datetime_value;
	    break;
	  case DB_SQL_TYPE_INTERVAL:
	    if (db_get_column_sqltype(colptr) & (DB_YEAR | DB_MONTH)) {
              if (db_test_value_datetime_current(value)) {
                db_error(
                 "db_set_value_datetime_current not valid for interval types");
                return DB_FAILED;
              }
	      sprintf(temp, "%d %d", db_get_value_year(value),
	        db_get_value_month(value));
	      if (incvfmtasc(temp, "%Y %m", &ym_interval_value) !=0) {
	        db_error("year/month interval conversion failed");
	        return DB_FAILED;
	      }
              $SET DESCRIPTOR $desc VALUE $dcol DATA = $ym_interval_value;
	      break;
	    }
	    else if (db_get_column_sqltype(colptr) & (DB_DAY | DB_HOUR |
	        DB_MINUTE | DB_SECOND | DB_FRACTION)) {
              if (db_test_value_datetime_current(value)) {
                db_error(
                 "db_set_value_datetime_current not valid for interval types");
                return DB_FAILED;
              }
	      sprintf(temp, "%d %d %d %.5lf", db_get_value_day(value),
	        db_get_value_hour(value), db_get_value_minute(value),
	        db_get_value_seconds(value));
	      if (incvfmtasc(temp, "%d %H %M %F5", &df_interval_value) !=0) {
	        db_error("day/fraction interval conversion failed");
	        return DB_FAILED;
	      }
              $SET DESCRIPTOR $desc VALUE $dcol DATA = $df_interval_value;
	      break;
	    }
	    else {
	      db_error("unknown interval type");
	      return DB_FAILED;
	      break;
	    }
	}
	break;
    }
    if (sql_error(NULL)) {
      return DB_FAILED;
    }
  }
  return DB_OK;
}
