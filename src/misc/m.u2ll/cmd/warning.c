#include "gis.h"
warning(buf, line, msg)
    char *buf, *msg;
{
    if (!isatty(0))
	fprintf (stderr, "%s: ** line %d <%s> %s **\n", G_program_name(), line, buf, msg);
    else
	fprintf (stderr, "** %s **\n", msg);
}

