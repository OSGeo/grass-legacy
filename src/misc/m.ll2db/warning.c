#include "gis.h"
#include <unistd.h>
int 
warning (char *buf, int line, char *msg)
{
    if (!isatty(0))
	fprintf (stderr, "%s: ** line %d <%s> %s **\n", G_program_name(), line, buf, msg);
    else
	fprintf (stderr, "** %s **\n", msg);

  return 0;
}

