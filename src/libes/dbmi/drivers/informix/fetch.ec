#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;

#include "globals.h"

/* NAME: db_driver_fetch
 * INPUT: cursor, position
 * OUTPUT: error code, dbTable with fetched data, end of file indicator
 * PROCESSING: FETCH {NEXT | PREVIOUS | CURRENT | FIRST | LAST } cursor USING 
 *  SQL DESCRIPTOR descriptor
 */
int
db_driver_fetch(cn, position, more)
    dbCursor *cn;
    int position;
    int *more;
{
    $char *cursor_name;
    $char *descriptor_name;
    $int int_value;
    $double double_value;
    $long  date_value;
    $datetime year to fraction(5) datetime_value;
    $interval year to month ym_interval_value;
    $interval day to fraction(5) df_interval_value;
    $int col;
    $int colIndicator;
    $string string_value[MAX_STRING_SIZE];
    $char char_value[MAX_STRING_SIZE];
    $int len;
    int type;
    dbToken token;
    cursor *c;
    char temp[128];
    int ncols;
    dbColumn *column;
    dbValue *value;
    int sqltype, Ctype;
    short dvec[3];
    dbTable *table;
    char *cp;

/* get cursor token */
    token = db_get_cursor_token(cn);

/* get the cursor by its token */
    if (!(c = (cursor *) db_find_token(token))) 
    {
	db_error("cursor not found");
	return DB_FAILED;
    }

/* get cursor name from cursor */
    cursor_name = c->cursor_name;

/* get descriptor name from cursor */
    descriptor_name = c->descriptor_name;

/* fetch on position */
    switch (position)
    { 
    case DB_NEXT:
	$FETCH NEXT $cursor_name USING SQL DESCRIPTOR $descriptor_name;
	break;
    case DB_CURRENT:
	$FETCH CURRENT $cursor_name USING SQL DESCRIPTOR $descriptor_name;
	break;
    case DB_PREVIOUS:
	$FETCH PREVIOUS $cursor_name USING SQL DESCRIPTOR $descriptor_name;
	break;
    case DB_FIRST:
	$FETCH FIRST $cursor_name USING SQL DESCRIPTOR $descriptor_name;
	break;
    case DB_LAST:
	$FETCH LAST $cursor_name USING SQL DESCRIPTOR $descriptor_name;
	break;
    };

/* eof? */
    if (sql_eof())
    {
	*more = 0;
	return DB_OK;
    }
    *more = 1;

/* if error, return error code */
    if (sql_error(NULL)) return DB_FAILED;

/* get the data out of the descriptor into the table */
    table = db_get_cursor_table(cn);
    ncols = db_get_table_number_of_columns (table);
    for (col = 1; col <= ncols; col++)
    {
	column = db_get_table_column (table, col-1);
	value  = db_get_column_value (column);
	$GET DESCRIPTOR $descriptor_name VALUE $col $colIndicator = INDICATOR;
	if (sql_error(NULL)) return DB_FAILED;
	if (colIndicator < 0)
	{
	    value->isNull = 1;
	    continue;
	}
	value->isNull = 0;

	sqltype = db_get_column_sqltype(column);
	Ctype   = db_sqltype_to_Ctype(sqltype);
	len     = db_get_column_length(column);
	type    = db_get_column_host_type(column);
	switch (Ctype)
	{
	case DB_C_TYPE_STRING:
	    if ((type & SQLTYPE) == SQLCHAR)
	    {
		$GET DESCRIPTOR $descriptor_name VALUE $col $char_value = DATA,
		  $len = LENGTH;
		if (sql_error(NULL)) return DB_FAILED;
		if (len < sizeof(char_value))
		    char_value[len] = 0;
		cp = char_value;
	    }
	    else if ((type & SQLTYPE) == SQLNULL) {
		cp = "<NULL DATA>";
	    }
	    else if ((type & SQLTYPE) == SQLBYTES) {
		cp = "<BYTE DATA>";
	    }
	    else if ((type & SQLTYPE) == SQLTEXT) {
		cp = "<TEXT DATA>";
	    }
	    else
	    {
		$GET DESCRIPTOR $descriptor_name VALUE $col $string_value = DATA;
		if (sql_error(NULL)) return DB_FAILED;
		cp = string_value;
	    }
	    db_set_string (&value->s, cp);
	    break;
	case DB_C_TYPE_INT:
	    $GET DESCRIPTOR $descriptor_name VALUE $col $int_value = DATA;
	    if (sql_error(NULL)) return DB_FAILED;
	    value->i = int_value;
	    break;
	case DB_C_TYPE_DOUBLE:
	    $GET DESCRIPTOR $descriptor_name VALUE $col $double_value = DATA;
	    if (sql_error(NULL)) return DB_FAILED;
	    value->d = double_value;
	    break;
	case DB_C_TYPE_DATETIME:
	    switch(sqltype & ~DB_DATETIME_MASK)
	    {
	    case DB_SQL_TYPE_DATE:
		$GET DESCRIPTOR $descriptor_name VALUE $col $date_value = DATA;
		if (sql_error(NULL)) return DB_FAILED;
		if (rjulmdy(date_value,dvec) != 0) 
		{
		   db_error("DATE conversion failed");
		   return DB_FAILED;
		}
		value->t.year = (int) dvec[2];
		value->t.month = (int) dvec[0];
		value->t.day = (int) dvec[1];
		value->t.hour = 0;
		value->t.minute = 0;
		value->t.seconds = 0.0;
		break;
	    case DB_SQL_TYPE_TIMESTAMP:
		$GET DESCRIPTOR $descriptor_name VALUE $col $datetime_value = 
		DATA;
		if (sql_error(NULL)) return DB_FAILED;
		if (dttofmtasc(&datetime_value,temp,sizeof(temp)-1,
		  "%Y %m %d %H %M %F5") != 0)
		{
		    db_error("datetime conversion failed");
		    return DB_FAILED;
		}
		sscanf(temp, "%d", &value->t.year);
		sscanf(temp, "%d %d %d %d %d %lf",
			&value->t.year, &value->t.month, &value->t.day,
			&value->t.hour, &value->t.minute, &value->t.seconds);
		break;
	    case DB_SQL_TYPE_INTERVAL:
		if (db_get_column_sqltype(column) & (DB_YEAR | DB_MONTH)) {
		  $GET DESCRIPTOR $descriptor_name VALUE $col 
		  $ym_interval_value = DATA;
		  if (sql_error(NULL)) return DB_FAILED;
		  if (intofmtasc(&ym_interval_value,temp,sizeof(temp)-1,
		    "%Y %m") != 0) {
		    db_error("year/month interval conversion failed");
		    return DB_FAILED;
		  }
		  sscanf(temp, "%d %d", &value->t.year, 
		    &value->t.month);
		  break;
		}
		else
		if (db_get_column_sqltype(column) & (DB_DAY | DB_HOUR | DB_MINUTE
		  | DB_SECOND | DB_FRACTION)) {
		  $GET DESCRIPTOR $descriptor_name VALUE $col 
		  $df_interval_value = DATA;
		  if (sql_error(NULL)) return DB_FAILED;
		  if (intofmtasc(&df_interval_value,temp,sizeof(temp)-1,
		    "%d %H %M %F5") != 0) {
		    db_error("day/fraction interval conversion failed");
		    return DB_FAILED;
		  }
		  sscanf(temp, "%d %d %d %lf",
		    &value->t.day, &value->t.hour, &value->t.minute, 
		    &value->t.seconds);
		}
		else {
                  db_error("unknown interval type");
		  return DB_FAILED;
		}
	    default: /* shouldn't happen */
		$GET DESCRIPTOR $descriptor_name VALUE $col $string_value = DATA;
		if (sql_error(NULL)) return DB_FAILED;
		db_set_string (&value->s, string_value);
		break;
	    }
	    break;
	}
    }
    return DB_OK;
}
