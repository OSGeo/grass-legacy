/*
 * This function returns the type of informix engine
 *
 * Present implementation:
 * If SQLEXEC isn't set, then the SE engine is assumed, else
 *	
 *	setenv SQLEXEC $INFORMIXDIR/lib/sqlexec		# SE engine
 *	setenv SQLEXEC $INFORMIXDIR/lib/sqlturbo	# Online engine
 */

#include "dbmi.h"
#include "globals.h"

static
char *
which_engine()
{
    char *getenv(), *sqlexec, *p;

    sqlexec = getenv ("SQLEXEC");
    if (sqlexec == NULL)
	return STANDARD_ENGINE;

    for (p = sqlexec; *p; p++)
	if (*p == '/')
	    sqlexec = p + 1;

    if (strcmp(sqlexec,"sqlturbo") == 0)
	return ONLINE_ENGINE;

    return STANDARD_ENGINE;
}

char *
informix_engine()
{
    static int first = 1;
    static char *engine;

    if (first)
    {
	engine = which_engine();
	first = 0;
    }
    return engine;
}

informix_engine_is_standard()
{
    return (strcmp(STANDARD_ENGINE, informix_engine()) == 0);
}

informix_engine_is_online()
{
    return (strcmp(ONLINE_ENGINE, informix_engine()) == 0);
}
