#include "gis.h"

G_done_msg(msg)
	char *msg;
{
	char *getlogin(), *user, *me;
	FILE *out;

	user = getlogin();
	if (user == NULL)
		return;
	me = G_whoami();
	if (me == NULL)
		return;
	if (strcmp(me,user) != 0)
		return;
	if (isatty(1))
		out = stdout;
	else if (isatty(2))
		out = stderr;
	else
		return;
	fprintf (out, "%s complete. %s\n", G_program_name(), msg);
}
