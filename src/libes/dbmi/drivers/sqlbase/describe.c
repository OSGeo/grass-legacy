#include "globals.h"

db_driver_describe_table (table_name, table)
    dbString *table_name;
    dbTable **table;
{
    char *name;

    if (!check_for_open_database())
	return DB_FAILED;

    name = db_get_string (table_name);

    if (describe_table (name, table) != DB_OK)
	return DB_FAILED;
    
/* column default values */
    get_table_default_values (*table);

/* table and column privs */
    get_table_privs (*table);

    return DB_OK;
}

describe_table (tableName, table)
    char *tableName;
    dbTable **table;
{
    char buf[1024];
    char *name;
    dbString *colNames;
    dbColumn *column;
    int col, ncols;
    int stat = DB_FAILED;

/* get list of columns */
    if (list_columns (tableName, &colNames, &ncols) != DB_OK)
	return DB_FAILED;

    if (ncols <= 0)
    {
	sprintf (buf, "Specified table (%s) not found", tableName);
	db_error (buf);
	return DB_FAILED;
    }

    *table = db_alloc_table (ncols);
    if (*table == NULL)
	goto done;

/* set the table name */
    db_set_table_name (*table, tableName);

/* set the table description */
    db_set_table_description(*table, get_remarks_for_table(tableName));

/* one column at a time, try to select from this column */
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (*table, col);
	name = db_get_string (&colNames[col]);
	sprintf (buf, "select %s from %s", name, tableName);
	db_set_column_name (column, name);
	db_set_column_description(column, get_remarks_for_column(tableName,name));
	if (sqlcom (database_cursor, buf, NULL_TERMINATED))
	{
	    db_set_column_select_priv_not_granted (column);
	}
	else
	{
	    db_set_column_select_priv_granted (column);
	    if(describe_column (database_cursor, column, 0) != DB_OK)
		goto done;
	}
    }
    stat = DB_OK;
done:
    db_free_string_array (colNames, ncols);
    return stat;;
}

describe (cursor, table)
    SQLTCUR cursor;
    dbTable **table;
{
    SQLTNSI nsi;	/* number of selected items */
    SQLTRCD rcd;	/* return code */
    int ncols, col;
    dbColumn *column;


    if(rcd = sqlnsi (cursor, &nsi))
    {
	report_error (rcd, "Can't get number of columns in query");
	return DB_FAILED;
    }
    ncols = nsi;
    *table = db_alloc_table (ncols);
    if(*table == NULL)
	return DB_FAILED;

    for (col = 0; col < nsi; col++)
    {
	column = db_get_table_column (*table, col);
	if(describe_column (cursor, column, col) != DB_OK)
	    return DB_FAILED;
    }
    return DB_OK;
}

describe_column (cursor, column, col)
    SQLTCUR cursor;
    dbColumn *column;
    int col;
{
    SQLTRCD rcd;	/* return code */
    SQLTGDI gdi;	/* describe structure */
    SQLTPDT pdt;	/* program data type */
    SQLTPDL pdl;	/* program data len */
    SQLTDAP pbp;	/* program data buffer */
    int dbtype;
    dbValue *value;


    db_zero (&gdi, sizeof(gdi)); /* a hack to fix SQLBASE buggy API */
    gdi.gdicol = col+1;
    if (rcd = sqlgdi (cursor, &gdi))
    {
	report_error (rcd, "Can't describe a column in the query");
	return DB_FAILED;
    }

    db_set_column_name (column, (char *) gdi.gdichb);
    db_set_column_scale (column, (int) gdi.gdisca);
    db_set_column_precision (column, (int) gdi.gdipre);
    db_set_column_length (column, (int) gdi.gdiedl);
    if (gdi.gdinul)
	db_set_column_null_allowed (column);
    else
	db_unset_column_null_allowed (column);
    db_set_column_host_type (column, (int) gdi.gdiedt);

    value = db_get_column_value (column);

    /* determine the DBMI datatype (based on SQLBASE types)
     * and bind the table value items for fetching into
     * see pg 373-375 C/API for sqlssb()
     */

    /* set table/bind to character strings */

    pdl = db_get_column_length (column);
    db_enlarge_string (&value->s,  (int)pdl + 1);
    pbp    = (SQLTDAP) db_get_string(&value->s);
    pdt    = SQLPBUF;

    switch (gdi.gdiedt)
    {
    case SQLEINT:
	dbtype = DB_SQL_TYPE_INTEGER;
	pdt    = SQLPSIN;
	pbp    = (SQLTDAP) &value->i;
	pdl    = sizeof(value->i);
	break;
    case SQLESMA:
	dbtype = DB_SQL_TYPE_SMALLINT;
	pdt    = SQLPSIN;
	pbp    = (SQLTDAP) &value->i;
	pdl    = sizeof(value->i);
	break;
    case SQLEFLO:
	if (db_get_column_length(column) > sizeof(float))
	    dbtype = DB_SQL_TYPE_DOUBLE_PRECISION;
	else
	    dbtype = DB_SQL_TYPE_REAL;
	pdt    = SQLPDOU;
	pbp    = (SQLTDAP) &value->d;
	pdl    = sizeof(value->d);
	break;
    case SQLECHR:
	dbtype = DB_SQL_TYPE_CHARACTER;
	pdl    = db_get_column_length (column);
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPBUF;
	break;
    case SQLEVAR:
	dbtype = DB_SQL_TYPE_CHARACTER;
	pdl    = db_get_column_length (column);
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPBUF;
	break;
    case SQLEDEC:
	dbtype = DB_SQL_TYPE_DECIMAL;
	pdl    = gdi.gdiddl+1;
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPBUF;
	break;
    case SQLEDAT:
	dbtype = DB_SQL_TYPE_DATE;
	pdl    = SQLSDTE;
	pdl    = SQLSCDA;
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPDTE;
	break;
    case SQLETIM:
	dbtype = DB_SQL_TYPE_TIME;
	pdl    = SQLSTIM;
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPTIM;
	break;
    case SQLETMS:
	dbtype = DB_SQL_TYPE_TIMESTAMP;
	pdl    = SQLSDAT;
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPDAT;
	break;
    case SQLEMON:
	dbtype = DB_SQL_TYPE_DECIMAL;
	pdl    = gdi.gdiddl+1;
	db_enlarge_string (&value->s,  (int)pdl + 1);
	pbp    = (SQLTDAP) db_get_string(&value->s);
	pdt    = SQLPBUF;
	break;
    case SQLEDOU:
	dbtype = DB_SQL_TYPE_DOUBLE_PRECISION;
	pdt    = SQLPDOU;
	pbp    = (SQLTDAP) &value->d;
	pdl    = sizeof(value->d);
	break;
    default:
	dbtype = DB_SQL_TYPE_UNKNOWN;
	pbp = SQLNPTR;
	pdl = 0;
	break;
    }
    db_set_column_sqltype (column, dbtype);

    /* bind table value for fetching */
    if (pbp != SQLNPTR)
	sqlssb (cursor, gdi.gdicol, pdt, pbp, pdl, 0, &column->dataLen, SQLNPTR);

    return DB_OK;
}
