#include "globals.h"

/* NAME: db_driver_add_column
 * INPUT: name of the table and a dbColumn describing what is to be added
 * OUTPUT: status
 * PROCESSING: ALTER TABLE table_name ADD column_name datatype  [ NOT NULL ]
 */
db_driver_add_column (tableName, column)
    dbString *tableName;
    dbColumn *column;
{
    dbString cmd;
    int stat;

    db_init_string(&cmd);
    db_append_string(&cmd, "ALTER TABLE ");
    db_append_string(&cmd, db_get_string(tableName));
    db_append_string(&cmd, " ADD ");
    db_append_string(&cmd, db_get_column_name(column));
    db_append_string(&cmd, " ");
    if(!build_column_type_info(column, &cmd))
    {
	stat = DB_FAILED;
	goto done;
    }
    if (!db_test_column_null_allowed(column))
	db_append_string(&cmd, " NOT NULL ");

    stat = execute_immediate (&cmd);
    if (stat != DB_OK)
	goto done;
    if (*db_get_column_description(column))
	add_remarks_to_column (
	    db_get_string(tableName),
	    db_get_column_name(column),
	    db_get_column_description(column)
	) ;
done:
    db_free_string(&cmd);
    return stat;
}

/* NAME: db_driver_drop_column
 * INPUT: the name of the table and the column to be removed from it
 * OUTPUT: status
 * PROCESSING: ALTER TABLE table_name DROP column_name
 */
db_driver_drop_column (tableName, columnName)
    dbString *tableName, *columnName;
{
    int stat;
    dbString cmd;

    db_init_string (&cmd);
    db_append_string(&cmd, "ALTER TABLE ");
    db_append_string (&cmd, db_get_string(tableName));
    db_append_string(&cmd, " DROP ");
    db_append_string (&cmd, db_get_string(columnName));

    stat = execute_immediate (&cmd);
    db_free_string (&cmd);
    return stat;
}

build_column_type_info(column, cmd)
    dbColumn *column;
    dbString *cmd;
{
    int sqltype;
    char buf[50];

    sqltype = db_get_column_sqltype(column);

/* NOTE: SERIAL and DATETIME INTERVAL types are not supported */
    if (sqltype == DB_SQL_TYPE_SERIAL)
    {
	db_error ("SERIAL data type is not supported");
	return 0;
    }
    if (sqltype & DB_DATETIME_MASK == DB_SQL_TYPE_INTERVAL)
    {
	db_error ("INTERVAL data type is not supported");
	return 0;
    }

/* TIMESTAMPS can not be intervals */
    if(sqltype & ~DB_DATETIME_MASK == DB_SQL_TYPE_TIMESTAMP)
	sqltype = DB_SQL_TYPE_TIMESTAMP;

    db_append_string(cmd, db_sqltype_name(sqltype));

    /* append the (precision, scale) or (length) if necessary */
    switch (sqltype) {
    case DB_SQL_TYPE_CHARACTER:
	sprintf(buf, "(%d)", db_get_column_length(column));
	db_append_string(cmd, buf);
	break;
    case DB_SQL_TYPE_DECIMAL:
    case DB_SQL_TYPE_NUMERIC:
	sprintf(buf, "(%d,%d)", db_get_column_precision(column),
	db_get_column_scale(column));
	db_append_string(cmd, buf);
	break;
    default: break;
    }
    return 1;
}

build_column_list (object, list, ncols, colname)
    void *object;
    dbString *list;
    int ncols;
    char * (*colname)();
{
    int col;

    for (col = 0; col < ncols; col++)
    {
	if(col)
	{
	    if(db_append_string (list, ",") != DB_OK)
		return DB_FAILED;
	}
	if(db_append_string (list, colname(object,col)) != DB_OK)
		return DB_FAILED;
    }
    return DB_OK;
}

list_columns (tableName, tlist, tcount)
    char *tableName;
    dbString **tlist;
    int *tcount;
{
    char tbname[256];
    char tbcreator[256];
    SQLTRCD rcd;
    dbString *list;
    int count;
    int eof, error;
    int pass;
    char select[600];
    char name[256];

    *tcount = 0;
    *tlist  = NULL;

    decompose_tablename (tableName, tbcreator, tbname);

/* build the select stmt */
    sprintf (select, "select name from sysadm.syscolumns where tbcreator = '%s' and tbname = '%s' order by colno", tbcreator, tbname);

/* compile the select stmt */
    if (rcd = sqlcom (database_cursor, select, NULL_TERMINATED))
    {
	report_error_with_carot(database_cursor, rcd, select);
	return DB_FAILED;
    }

/* tell fetch where to put the name */
    if (bind_string_for_fetch (database_cursor,
	    1,		/* column number */
	    name,		/* program buffer */
	    sizeof(name),	/* program buffer length */
	    SQLNPTR		/* fetch code status */
	))
    {
	return DB_FAILED;
    }


/* fetch the names, once to get a count, again to get the names */
    for (pass = 0; pass < 2; pass++)
    {
	if (rcd = sqlexe (database_cursor))
	{
	    report_error (rcd, "list_columns sqlexe() failure");
	    return DB_FAILED;
	}
	eof = error = 0;
	for (count = 0; !(eof || error); count++)
	{
	    db_zero (name, sizeof(name));
	    rcd = sqlfet(database_cursor);
	    error = fetch_error(rcd);
	    eof   = fetch_eof (rcd);
	    if (error || eof)
		break;
	    if (pass==1)
	    {
		if (db_set_string (&list[count], name) != DB_OK)
		    return DB_FAILED;
	    }
	}
	if (error)
	{
	    report_error (rcd, "list_columns sqlfet() failure");
	    return DB_FAILED;
	}
	if(pass==0)
	{
	    list = db_alloc_string_array(count);
	    if (list == NULL)
		return DB_FAILED;
	}
    }

    *tlist = list;
    *tcount = count;
    return DB_OK;
}
