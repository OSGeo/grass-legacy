#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;

/*******************************************/
/*  Server State                           */
/*******************************************/
#include "globals.h"

free_descriptor(descriptor_name)
    $parameter char *descriptor_name;
{
    /* deallocate the system descriptor */
    $DEALLOCATE DESCRIPTOR $descriptor_name;
    if (sql_error(NULL))
	return DB_FAILED;
    return DB_OK;
}

/* NAME: alloc_descriptor_data
 * INPUT: descriptor whose data areas are to be allocated
 * OUTPUT: error code
 * PROCESSING: walk through the descriptor data, allocating and filling in a 
 *  dbTable 
 */
int
alloc_descriptor_data(descriptor_name, table)
    $parameter char *descriptor_name;
    dbTable **table;
{
    dbColumn *curCol;
    int col;
    $int colCount;

    /* get the number of colummns from the descriptor */
    $GET DESCRIPTOR $descriptor_name $colCount = COUNT;
    if (sql_error(NULL))
	return DB_FAILED;

    /* allocate a table structure to correspond to our system descriptor */
    if (!(*table = db_alloc_table(colCount))) {
	return DB_FAILED;
    }

    /* for each column in the descriptor */
    for (col = 1; col <= colCount; col++) {
      if(describe_column(descriptor_name, col, 
                         db_get_table_column(*table, col - 1)) != DB_OK) {
        db_free_table(*table);
        *table = NULL;
        return DB_FAILED;
      }
    }
    return DB_OK;
}

describe_column(desc_name, colno, column)
$parameter char *desc_name;
$parameter int colno;
dbColumn *column;
{
  int dbType;
  $int colType, colLength, colPrecision, colScale, colNullAllowed;
  $string colName[200];
#define DTENCODE(qual) (DTSTART[TU_START((qual))>>1] & \
                        DTEND[TU_END((qual))>>1])
  static int DTSTART[] = { 0x7F00, 0x3F00, 0x1F00, 0x0F00, 0x0700, 0x0300, 
			     0x0100 };
  static int DTEND[] = { 0x4000, 0x6000, 0x7000, 0x7800, 0x7C00, 0x7E00, 
			     0x07F00 };

  $GET DESCRIPTOR $desc_name VALUE $colno $colName = NAME, $colType = TYPE,
    $colLength = LENGTH, $colPrecision = PRECISION, $colScale = SCALE, 
    $colNullAllowed = NULLABLE;

  if (sql_error(NULL)) return DB_FAILED;

  /* set the column name */
  db_set_column_name(column, colName);

  /* set the other info */
  colNullAllowed ? db_set_column_null_allowed(column) : 
                   db_unset_column_null_allowed(column);
  db_set_column_precision(column, colPrecision);
  db_set_column_scale(column, colScale);
  db_set_column_length(column, colLength);
  db_set_column_host_type(column, colType);

  /* determine data type */
  switch (colType & SQLTYPE) {
    case SQLCHAR:
    case SQLVCHAR:    dbType = DB_SQL_TYPE_CHARACTER;        break;
    case SQLSMINT:    dbType = DB_SQL_TYPE_SMALLINT;         break;
    case SQLINT:      dbType = DB_SQL_TYPE_INTEGER;          break;
    case SQLFLOAT:    dbType = DB_SQL_TYPE_DOUBLE_PRECISION; break;
    case SQLSMFLOAT:  dbType = DB_SQL_TYPE_REAL;             break;
    case SQLDECIMAL:  dbType = DB_SQL_TYPE_DECIMAL;          break;
    case SQLSERIAL:   dbType = DB_SQL_TYPE_SERIAL;           break;
    case SQLDATE:     dbType = DB_SQL_TYPE_DATE;             break;
    case SQLMONEY:    dbType = DB_SQL_TYPE_DECIMAL;          break;
    case SQLNULL:     dbType = DB_SQL_TYPE_UNKNOWN;          break;
    case SQLDTIME:    dbType = DB_SQL_TYPE_TIMESTAMP
			       | DTENCODE(colLength);        break;

    case SQLBYTES:    dbType = DB_SQL_TYPE_UNKNOWN;          break;
    case SQLTEXT:     dbType = DB_SQL_TYPE_UNKNOWN;          break;
    case SQLINTERVAL: dbType = DB_SQL_TYPE_INTERVAL
			       | DTENCODE(colLength);        break;
    default:          dbType = DB_SQL_TYPE_UNKNOWN;          break;
  }
  db_set_column_sqltype(column, dbType);
  return DB_OK;
}
