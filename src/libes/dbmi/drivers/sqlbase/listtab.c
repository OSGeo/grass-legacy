#include "globals.h"

db_driver_list_tables (tlist, tcount, system)
    dbString **tlist;
    int *tcount;
{
    dbString *list;
    int count, pass;
    SQLTRCD rcd;
    SQLTFSC fc_name, fc_creator;	/* fetch status (null indicator) */
    char name[300];
    char creator[300];
    char fullname[600];
    char select[600];
    int eof, error;

#define CREATOR_COL 1
#define NAME_COL 2

    *tlist = NULL;
    *tcount = 0;

    if (!check_for_open_database())
	return DB_FAILED;

    sprintf (select, "SELECT CREATOR, NAME FROM SYSADM.SYSTABLES WHERE CREATOR %s IN ('SYSADM', 'SYSSQL')", system ? "": "NOT") ;

/* compile the select which gives the list of names */
    if (rcd = sqlcom (database_cursor, select, NULL_TERMINATED))
    {
	report_error (rcd, "db_list_tables sqlcom() failure");
	return DB_FAILED;
    }

/* tell the fetch where to put the names */
    if (bind_string_for_fetch (database_cursor,
	    NAME_COL,		/* column number */
	    name,		/* program buffer */
	    sizeof(name),	/* program buffer length */
	    &fc_name		/* fetch code status */
	))
    {
	return DB_FAILED;
    }

    if (bind_string_for_fetch (database_cursor,
	    CREATOR_COL,	/* column number */
	    creator,		/* program buffer */
	    sizeof(creator),	/* program buffer length */
	    &fc_creator		/* fetch code status */
	))
    {
	return DB_FAILED;
    }

/* Fetch the names once to get a count, and again to get the names */

    for (pass = 0; pass < 2; pass++)
    {
	if (rcd = sqlexe (database_cursor))
	{
	    report_error (rcd, "db_list_tables sqlexe() failure");
	    return DB_FAILED;
	}

	eof = 0;
	error = 0;
	for (count = 0; !(eof || error) ; count++)
	{
	    db_zero (name, sizeof(name));
	    db_zero (creator, sizeof(creator));
	    rcd = sqlfet(database_cursor);
	    error = fetch_error(rcd);
	    eof = fetch_eof(rcd);
	    if (error  || eof)
		break;
	    if (pass==1)
	    {
		*fullname = 0;
		if (!is_null_value(fc_name))
		    sprintf (fullname,"%s.", creator);
		strcat (fullname, name);
		if(db_set_string (&list[count], fullname) != DB_OK)
		    return DB_FAILED;
	    }
	}
	if(error)
	{
	    report_error (rcd, "db_list_tables sqlfet() failure");
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
