#include <grass/gis.h>
#include <grass/glocale.h>
#include <stdarg.h>

int G_done_msg(const char *msg, ...)
{
	char buffer[2000];
	va_list ap;

	va_start(ap, msg);
	vsprintf(buffer,msg,ap);
	va_end(ap);

	G_message(_("%s complete. %s"), G_program_name(), buffer);

	return 0;
}
