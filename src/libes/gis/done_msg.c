#include <unistd.h>
#include "gis.h"
#include "glocale.h"
#include <string.h>

int G_done_msg(char *msg)
{
	char *me;
	FILE *out;

#ifdef __MINGW32__
        static char *user = "mingw_user_name";
#else
	char *user = getlogin();
#endif
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
