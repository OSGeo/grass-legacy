#include "globals.h"

static void grant_select();

/* NAME: db_driver_create_database 
 * INPUT:
 * OUTPUT:
 * PROCESSING:
 */
db_driver_create_database (handle)
    dbHandle *handle;
{
    SQLTRCD rcd;	/* return code */
    SQLTSVH svh;	/* server handle for this new database */
    SQLTCUR dbcur;	/* database cursor for this new database */
    char *dbname, *server;
    char *user;
    char buf[256];
    char password[SQLMFNL+1];
    SQLTDAL len;

    dbcur = 0;
    svh = 0;

/* get the default password */
    db_zero (password, sizeof(password));
    len = 0; /* paranoia */
    sqlget (0, SQLPDPW, password, &len);
    if (*password == 0)
	strcpy (password, "SYSADM");

    user = whoami();

    dbname = db_get_handle_dbname(handle);
    server = db_get_handle_dbpath(handle);
    if (*server == 0) server = NULL; /* use default server */

    if(connect_to_server (server, &svh) != DB_OK)
	return DB_FAILED;

/* create the database */
    if (rcd = sqlcre (svh, dbname, NULL_TERMINATED))
    {
	sprintf (buf, "Unable to create database (%s)", dbname);
	report_error (rcd, buf);
	goto fail;
    }

/* connect to this new database */

    if(rcd = sqlcnc(&dbcur, dbname, NULL_TERMINATED))
    {
	sprintf (buf, "Database (%s) created, but unable to open it", dbname);
	report_error (rcd, buf);
	goto fail;
    }

/* grant CONNECT priv and set password to the default  */

    sprintf (buf, "GRANT CONNECT TO %s IDENTIFIED BY %s", user, password);
    if (rcd = sqlcex (dbcur, buf, NULL_TERMINATED))
    {
	sprintf (buf, "Database (%s) created, but unable to grant CONNECT privileges to (%s)", dbname, user);
	report_error (rcd, buf);
	goto fail;
    }

/* grant DBA priv */

    sprintf (buf, "GRANT DBA TO %s", user);
    if (rcd = sqlcex (dbcur, buf, NULL_TERMINATED))
    {
	sprintf (buf, "Database (%s) created, but unable to grant DBA privileges to (%s)", dbname, user);
	report_error (rcd, buf);
	goto fail;
    }

/* grant SELECT to PUBLIC on many system tables */

    grant_select (dbcur, "SYSTABLES");
    grant_select (dbcur, "SYSCOLUMNS");
    grant_select (dbcur, "SYSVIEWS");
    grant_select (dbcur, "SYSINDEXES");
    grant_select (dbcur, "SYSKEYS");

    grant_select (dbcur, "SYSCOLAUTH");
    grant_select (dbcur, "SYSTABAUTH");
    grant_select (dbcur, "SYSUSERAUTH");

    grant_select (dbcur, "SYSTABCONSTRAINTS");
    grant_select (dbcur, "SYSPKCONSTRAINTS");
    grant_select (dbcur, "SYSFKCONSTRAINTS");

    disconnect_from_database(&dbcur);
    disconnect_from_server(&svh);
    return DB_OK;
fail:
    disconnect_from_database(&dbcur);
    disconnect_from_server(&svh);
    return DB_FAILED;
}

static
void
grant_select (dbcur, name)
    SQLTCUR dbcur;
    char *name;
{
    char cmd[1024];

    sprintf (cmd, "grant select on SYSADM.%s to public", name);
    sqlcex (dbcur, cmd, NULL_TERMINATED);
}
