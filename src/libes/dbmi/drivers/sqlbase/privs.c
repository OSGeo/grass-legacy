#include "globals.h"

/* determine the table/column privs for the current user
 *
 * if the privs can be determined, then they will be set to either to 
 *    DB_GRANTED or DB_NOT_GRANTED
 * otherwise they will not be set. 
 */

static char insertauth[128];
static char deleteauth[128];
static char selectauth[128];
static char updateauth[128];
static char updatecols[128];
static char tbname[128];
static char tbcreator[128];

void
get_table_privs (table)
    dbTable *table;
{
    char user[256];
    int stat;
    dbColumn *column;
    int col, ncols;

    get_database_user (user);
    decompose_tablename (db_get_table_name (table), tbcreator, tbname);

/* get the privs for USER
 *
 * if USER is a DBA, assume granted
 * if USER owns the table
 *   if no explicit privs listed, assume granted
 * if USER doesn't own the table
 *   if no explicit privs listed, get those for PUBLIC
 *
 * NOTE: a DBA has special status - all privs.
 *       Can a DBA be denied privs?
 */
    if(user_is_dba(user))                    /* is user a DBA? */
    {
	grant_all_table_privs (table);
	return;
    }

    stat = table_privs (user);
    if (stat == -1) return;	/* assume not allowed to query system tables */
    if (stat == 0)		/* no entry for this user */
    {
	if (db_nocase_compare(tbcreator, user))   /* does user own this table? */
	{
	    grant_all_table_privs (table);
	    return;
	}
	strcpy(user, "PUBLIC");
	stat = table_privs (user);
    }
    if (stat == -1) return;	/* assume not allowed to query system tables */
    if (stat == 0)		/* no entry for this user */
    {
	deny_all_table_privs (table);
	return;
    }

    if(*selectauth == 'Y')
	db_set_table_select_priv_granted(table);
    else
	db_set_table_select_priv_not_granted(table);

    if(*insertauth == 'Y')
	db_set_table_insert_priv_granted(table);
    else
	db_set_table_insert_priv_not_granted(table);

    if(*deleteauth == 'Y')
	db_set_table_delete_priv_granted(table);
    else
	db_set_table_delete_priv_not_granted(table);

/* UPDATE is different - update is a column level priv */
    if (*updateauth != 'Y')
	db_set_table_update_priv_not_granted(table);
    else if (*updatecols != '*')
	db_set_table_update_priv_granted(table);
    else
    {
	ncols = db_get_table_number_of_columns(table);
	for (col = 0; col < ncols; col++)
	{
	    column = db_get_table_column (table, col);
	    switch(column_update_priv (db_get_column_name(column), user))
	    {
	    case 1: db_set_column_update_priv_granted(column); break;
	    case 0: db_set_column_update_priv_not_granted(column); break;
	    default: break; /* do nothing */
	    }
	}
    }
}

void
grant_all_table_privs (table)
    dbTable *table;
{
    db_set_table_select_priv_granted(table);
    db_set_table_update_priv_granted(table);
    db_set_table_insert_priv_granted(table);
    db_set_table_delete_priv_granted(table);
}

void
deny_all_table_privs (table)
    dbTable *table;
{
    db_set_table_select_priv_not_granted(table);
    db_set_table_update_priv_not_granted(table);
    db_set_table_insert_priv_not_granted(table);
    db_set_table_delete_priv_not_granted(table);
}

/* return -1: can't execute the query
 *         0: no info for this user
 *         1: OK.
 */
static int
table_privs (user)
    char *user;
{
    char cmd[256];
    SQLTRCD rcd;	/* return code */

    sprintf (cmd,
	"select updatecols, deleteauth, insertauth, selectauth, updateauth from syssql.systabauth where grantee = '%s' and ttname = '%s' and tcreator = '%s'",
	user, tbname, tbcreator);

    if(sqlcom (database_cursor, cmd, NULL_TERMINATED))
	return -1;


    db_zero (updatecols, sizeof(updatecols));
    if(bind_string_for_fetch (database_cursor,
	    1,		/* column number */
	    updatecols,	/* program buffer to hold results */
	    sizeof(updatecols),
	    SQLNPTR	/* fetch code status */
    ))
    {
	return -1;
    }

    db_zero (deleteauth, sizeof(deleteauth));
    if(bind_string_for_fetch (database_cursor,
	    2,		/* column number */
	    deleteauth,	/* program buffer to hold results */
	    sizeof(deleteauth),
	    SQLNPTR	/* fetch code status */
    ))
    {
	return -1;
    }

    db_zero (insertauth, sizeof(insertauth));
    if(bind_string_for_fetch (database_cursor,
	    3,		/* column number */
	    insertauth,	/* program buffer to hold results */
	    sizeof(insertauth),
	    SQLNPTR	/* fetch code status */
    ))
    {
	return -1;
    }

    db_zero (selectauth, sizeof(selectauth));
    if(bind_string_for_fetch (database_cursor,
	    4,		/* column number */
	    selectauth,	/* program buffer to hold results */
	    sizeof(selectauth),
	    SQLNPTR	/* fetch code status */
    ))
    {
	return -1;
    }

    db_zero (updateauth, sizeof(updateauth));
    if(bind_string_for_fetch (database_cursor,
	    5,		/* column number */
	    updateauth,	/* program buffer to hold results */
	    sizeof(updateauth),
	    SQLNPTR	/* fetch code status */
    ))
    {
	return -1;
    }

    if(sqlexe (database_cursor))
	return -1;

    rcd = sqlfet (database_cursor);
    if (fetch_error(rcd))
	return -1;
    return fetch_eof(rcd) ? 0 : 1;
}

/* return -1: can't execute the query
 *         0: no info for this column,user
 *         1: GRANTED
 */
static int
column_update_priv (colname, user)
    char *colname;
    char *user;
{
    char cmd[256];
    SQLTRCD rcd;	/* return code */
    int count;

    sprintf (cmd,
	"select count(*) from syssql.syscolauth where grantee = '%s' and tname = '%s' and creator = '%s' and colname = '%s'",
	user, tbname, tbcreator, colname);

    if(sqlcom (database_cursor, cmd, NULL_TERMINATED))
	return -1;


    if(bind_int_for_fetch (database_cursor,
	    1,		/* column number */
	    &count,	/* program buffer to hold results */
	    SQLNPTR	/* fetch code status */
    ))
    {
	return -1;
    }


    if(sqlexe (database_cursor))
	return -1;

    rcd = sqlfet (database_cursor);
    if (fetch_error(rcd))
	return -1;
    if(fetch_eof(rcd))
	return -1;
    return (count ? 1 : 0);
}

user_is_dba (user)
    char *user;
{
    int count;
    char cmd[1024];

    sprintf (cmd,
	"select count(*) from sysadm.sysuserauth where name = '%s' and (dbaauth = 'G' or dbaauth = 'Y')",
	user);
    if(sqlcom (database_cursor, cmd, NULL_TERMINATED))
	return 0;
    if(bind_int_for_fetch (database_cursor,
	1,
	&count,
	SQLNPTR
    ))
    {
	return 0;
    }

    if(sqlexe (database_cursor))
	return 0;

    if (sqlfet (database_cursor))
	return 0;

    return count;
}
