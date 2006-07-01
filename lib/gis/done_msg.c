#include <grass/gis.h>
#include <grass/glocale.h>

int G_done_msg(char *msg)
{
	G_message(_("%s complete. %s"), G_program_name(), msg);

	return 0;
}
