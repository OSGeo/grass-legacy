#include "globals.h"

int
db_driver_fetch(dbc, position, more)
    dbCursor *dbc;
    int position;
    int *more;
{
    mycursor *cursor;
    dbTable *table;
    dbColumn *column;
    dbValue *value;
    int ncols;
    SQLTRCD rcd;	/* SQL return codes */
    SQLTSLC col;
    SQLTCDL cvl;	/* column data length */
    SQLTFSC fc;		/* fetch status */

    cursor = (mycursor *) db_find_token (db_get_cursor_token (dbc));
    if (cursor == NULL)
    {
	db_error ("invalid cursor");
	return DB_FAILED;
    }

    table = db_get_cursor_table (dbc);
    ncols = db_get_table_number_of_columns (table);

    /* zero out the string in the value structure for each column 
     * this depends on describe() allocating the string
     */
    for (col = 0 ; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	value  = db_get_column_value (column);
	db_zero_string (&value->s);
    }

    switch (position)
    {
    case DB_NEXT:
	rcd = 0;
	break;
    case DB_FIRST:
	cursor->next_row = 0;
	rcd = sqlprs (cursor->database_cursor, cursor->next_row);
	break;
    case DB_LAST:
	cursor->next_row = cursor->nrows - 1;
	rcd = sqlprs (cursor->database_cursor, cursor->next_row);
	break;
    case DB_PREVIOUS:
	cursor->next_row -= 2;
	rcd = sqlprs (cursor->database_cursor, cursor->next_row);
	break;
    case DB_CURRENT:
	cursor->next_row -= 1;
	rcd = sqlprs (cursor->database_cursor, cursor->next_row);
	break;
    }
    if (rcd)
    {
	report_error (rcd, "sqlprs()");
	return DB_FAILED;
    }

    cursor->next_row++;	/* increment next row to be fetched */
    if (rcd = sqlfet (cursor->database_cursor))
    {
	if (fetch_eof(rcd))
	{
	    *more = 0;
	    return DB_OK;
	}
	if (fetch_error(rcd))
	{
	    report_error (rcd, "sqlfet()");
	    return DB_FAILED;
	}
    }

    /* get fetch info for each col */
    for (col = 0; col < ncols; col++)
    {
	rcd = sqlgfi (cursor->database_cursor, col+1, &cvl, &fc);
	if (rcd)
	{
	    report_error (rcd, "sqlgfi()");
	    return DB_FAILED;
	}
	column = db_get_table_column (table, col);
	value  = db_get_column_value (column);
	if (cvl == 0)
	{
	    db_set_value_null (value);
	}
	else
	{
	    db_set_value_not_null (value);
	    if (is_datetime(column))
	    {
		if(!convert_datetime (column))
		    return DB_FAILED;
	    }
	}
    }
    *more = 1;
    return DB_OK;
}

/* 0 row fetched
 * 1 eof
 * 2 update performed since last fetch
 * 3 delete preformed since last fetch
 */
fetch_eof(rcd)
{
    return rcd == 1;
}
fetch_error(rcd)
{
    return rcd > 3;
}

is_datetime (column)
    dbColumn *column;
{
    int sqltype, Ctype;

    sqltype = db_get_column_sqltype (column);
    Ctype  = db_sqltype_to_Ctype (sqltype);
    return Ctype == DB_C_TYPE_DATETIME;
}

convert_datetime (column)
    dbColumn *column;
{
    dbValue *value;
    SQLTRCD rcd;
    SQLTNMP ilp;
    char buf[256];
    int year, month, day, hour, min; 
    double sec;

    switch (db_get_column_host_type (column))
    {
    case SQLEDAT: break;
    case SQLETIM: break;
    case SQLETMS: break;
    default:
	db_error ("convert_datetime() called with non-datetime data");
	return 0;
    }

/* this is type coersion - needed since SQLTNMP is not an int*
 * note the describe() tells the fetch to put the length of the fetched
 * data into column->dataLen (is this a hack, or what )
 */
    ilp=(SQLTNMP) &column->dataLen;

    value = db_get_column_value (column);
    rcd = sqlxdp (buf, sizeof(buf)-1, db_get_value_string(value), *ilp,
	          "YYYY MM DD HH MI SS.999999", NULL_TERMINATED);
    if (rcd)
    {
	report_error (rcd, "sqlxdp()");
	return 0;
    }

    year = month = day = 0;
    hour = min = 0;
    sec = 0;

    sscanf (buf, "%d %d %d %d %d %lf", &year, &month, &day, &hour, &min, &sec);

    db_set_value_year (value, year);
    db_set_value_month (value, month);
    db_set_value_day (value, day);
    db_set_value_hour (value, hour);
    db_set_value_minute (value, min);
    db_set_value_seconds (value, sec);

    return 1;
}

SQLTRCD
fetch (cursor, eof)
    SQLTCUR cursor;
    int *eof;
{
    SQLTRCD rcd;

    *eof = 0;
    rcd = sqlfet (cursor);
    if (fetch_eof(rcd))
    {
	*eof = 1;
	rcd = 0;
    }
    else if (!fetch_error(rcd))
	rcd = 0;
    
    if (rcd)
	report_error (rcd, "sqlfet() fetch error");
    return rcd;
}
