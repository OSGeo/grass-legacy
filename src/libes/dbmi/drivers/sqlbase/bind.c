#include "globals.h"

SQLTRCD
bind_int_for_fetch (cursor, bindcol, x, fc)
    SQLTCUR cursor;
    int bindcol;
    int *x;
    SQLTFSC *fc;
{
    SQLTRCD rcd;
    if (rcd = sqlssb (cursor,
	    bindcol,		/* column number */
	    SQLPSIN,		/* program data type: string */
	    x,			/* program buffer */
	    sizeof(*x),		/* program buffer length */
	    0,			/* scale of packed decimal data */
	    SQLNPTR,		/* current value length */
	    fc			/* fetch code status */
	))
    {
	bind_for_fetch_failure (rcd);
    }
    return rcd;
}

SQLTRCD
bind_string_for_fetch (cursor, bindcol, string, size, fc)
    SQLTCUR cursor;
    int bindcol;
    char *string;
    int size;
    SQLTFSC *fc;
{
    SQLTRCD rcd;
    if (rcd = sqlssb (cursor,
	    bindcol,		/* column number */
	    SQLPSTR,		/* program data type: string */
	    string,		/* program buffer */
	    size-1,		/* program buffer length */
	    0,			/* scale of packed decimal data */
	    SQLNPTR,		/* current value length */
	    fc			/* fetch code status */
	))
    {
	bind_for_fetch_failure(rcd);
    }
    return rcd;
}

bind_for_fetch_failure(rcd)
    SQLTRCD rcd;
{
    report_error (rcd, "sqlssb() bind failure");
}

SQLTRCD
bind_column_for_insert (cursor, bindvar, column)
    SQLTCUR cursor;
    int bindvar;
    dbColumn *column;
{
    return bind_column_for_update (cursor, bindvar, column);
}

SQLTRCD
bind_column_for_update (cursor, bindvar, column)
    SQLTCUR cursor;
    int bindvar;
    dbColumn *column;
{
    SQLTRCD rcd = 0;
    SQLTBNN bnn;	/* bind variable number */
    SQLTDAP dap;	/* data buffer */
    SQLTDAL dal;	/* data buffer length */
    SQLTSCA sca = 0;	/* scale of packed decimal data */
    SQLTPDT pdt;	/* program data type */
    SQLTNML ol;		/* a length */
    dbValue *value;
    int isnull;
    char buf[1024];


    value = db_get_column_value (column);
    isnull = db_test_value_isnull(value);

    bnn = bindvar;
    switch (db_sqltype_to_Ctype(db_get_column_sqltype (column)))
    {
    case DB_C_TYPE_INT:
	    pdt = SQLPSIN;
	    dal = sizeof(int);
	    dap = (SQLTDAP) &value->i;
	    break;

    case DB_C_TYPE_DOUBLE:
	    pdt = SQLPDOU;
	    dal = sizeof(double);
	    dap = (SQLTDAP) &value->d;
	    break;

    case DB_C_TYPE_DATETIME:
	    pdt = SQLPDAT;
	    db_enlarge_string (&value->s, (int) SQLSCDA + 1);
	    dap = (SQLTDAP) db_get_string(&value->s);

	    if(db_test_value_datetime_current(value))
	    {
		rcd = current_datetime (dap, &ol);
	    }
	    else
	    {
		sprintf (buf, "%04d %02d %02d %02d %02d %02.6lf",
		    db_get_value_year(value),
		    db_get_value_month(value),
		    db_get_value_day(value),
		    db_get_value_hour(value),
		    db_get_value_minute(value),
		    db_get_value_seconds(value));

		rcd = sqlxpd (dap, &ol, buf, "YYYY MM DD HH MI SS.999999", 0);
	    }
	    dal = ol;
	    break;

    case DB_C_TYPE_STRING:
	    pdt = SQLPSTR;
	    if (isnull)
		db_set_string (&value->s, "");
	    dal = 0; /* let sqlbase figure out the length */
	    dap = (SQLTDAP) db_get_string(&value->s);
	    break;

    default:
	    db_error ("bind_column_for_update: shouldn't happen - unknown C-type");
	    return -1;
    }
    if (isnull)
	dal = 0;

    rcd = sqlbnn (cursor, bnn, dap, dal, sca, pdt);
    if (rcd)
	report_error (rcd, "sqlbnn() bind failure");
    return rcd;
}
