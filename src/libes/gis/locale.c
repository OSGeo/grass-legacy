
#include "config.h"
#include "glocale.h"

#include <stdlib.h>
#include <string.h>
#include <locale.h>

static char localedir[4096];

static char *
locale_dir(void)
{
	const char *gisbase;

	if (*localedir)
		return localedir;

	gisbase = getenv("GISBASE");
	if (!gisbase || !*gisbase)
		return "";

	strcpy(localedir, gisbase);
	strcat(localedir, "/locale");

	return localedir;
}

void
G_init_locale(void)
{
#ifdef HAVE_LIBINTL_H
	setlocale(LC_MESSAGES, "");
#endif
}

char *
G_gettext(const char *package, const char *msgid)
{
	static char now_bound[4096];

	if (strcmp(now_bound, package) != 0)
	{
		strcpy(now_bound, package);
		bindtextdomain(package, locale_dir());
	}

	return dgettext(package, msgid);
}

