#include <unistd.h>
#include "gis.h"
#include "glocale.h"
#include <string.h>

int G_done_msg(char *msg)
{
	char *user, *me;
	FILE *out;

	user = getlogin();
	if (user == NULL)
		return 1;
	me = G_whoami();
	if (me == NULL)
		return 1;
	if (strcmp(me,user) != 0)
		return 1;
	if (isatty(1))
		out = stdout;
	else if (isatty(2))
		out = stderr;
	else
		return 1;
	fprintf (out, _("%s complete. %s\n"), G_program_name(), msg);

	return 0;
}
