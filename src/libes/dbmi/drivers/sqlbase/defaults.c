#include "globals.h"

/* SQLBASE default values are limited to
 *   0                 if column is numeric
 *   one blank         if column is character
 *   current date/time if column is date/time
 *
 * select count(*) from SYSADM.SYSCOLUMNS or SYSSQL.SYSCOLUMNS where NULLS='D'
 */
void
get_table_default_values (table)
    dbTable *table;
{
    char tbname[128];
    char tbcreator[128];
    int col, ncols;
    dbColumn *column;


    decompose_tablename (db_get_table_name (table), tbcreator, tbname);
    ncols = db_get_table_number_of_columns (table);
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column(table, col);
	switch (column_has_default (tbname, tbcreator, db_get_column_name(column)))
	{
	case 1: /* has a default */
		set_column_default (column);
		break;
	case 0: /* doesn't have a default */
		break;
	default: /* unknown */
		break;
	}
    }
}

column_has_default (tbname, tbcreator, colname)
    char *tbname;
    char *tbcreator;
    char *colname;
{
SQLTRCD rcd;
    char cmd[1024];
    int count;

    sprintf (cmd,
	"select count(*) from syssql.syscolumns where tbcreator = '%s' and tbname = '%s' and name = '%s' and nulls = 'D'",
	tbcreator, tbname, colname);

    if(rcd = sqlcom (database_cursor, cmd, NULL_TERMINATED))
    {
report_error_with_carot(database_cursor, rcd, cmd);
        return -1;
    }

    if(bind_int_for_fetch (database_cursor,
            1,          /* column number of count(*) */
            &count,     /* program buffer to hold results */
            SQLNPTR     /* fetch code status */
    ))
    { 
        return -1;
    }

    if(sqlexe (database_cursor))
        return -1;

    if (sqlfet (database_cursor))
        return -1;

    return count ? 1 : 0;
}

void
set_column_default (column)
    dbColumn *column;
{
    dbValue *value;
    int sqltype, Ctype;

    sqltype = db_get_column_sqltype(column);
    Ctype = db_sqltype_to_Ctype(sqltype);

    value = db_get_column_default_value(column);

    if (sqltype == DB_SQL_TYPE_CHARACTER)
    {
	db_set_value_string (value, " ");
    }
    else if (Ctype == DB_C_TYPE_DATETIME)
    {
	db_set_value_datetime_current (value);
    }
    else
    {
	db_convert_Cstring_to_value ("0", sqltype, value);
    }
    db_set_column_has_defined_default_value(column);
}
