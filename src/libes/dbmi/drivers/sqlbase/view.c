#include "globals.h"

/* determine if a name is a view or not */

is_view(name)
    char *name;
{
    int count;
    char tbname[256];
    char tbcreator[256];
    char cmd[256];

    decompose_tablename (name, tbcreator, tbname);

    sprintf (cmd, "select count(*) from sysadm.sysviews where name = '%s' and creator = '%s'", tbname, tbcreator);

    if(sqlcom (database_cursor, cmd, NULL_TERMINATED))
	return 0;

    if(bind_int_for_fetch (database_cursor,
	    1,		/* column number of count(*) */
	    &count,	/* program buffer to hold results */
	    SQLNPTR	/* fetch code status */
    ))
    {
	return 0;
    }

    if(sqlexe (database_cursor))
	return 0;

    if (sqlfet (database_cursor))
	return 0;

    return count ? 1 : 0;
}
