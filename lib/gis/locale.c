
#include "config.h"
#include "glocale.h"

#include <stdio.h>
#include <stdlib.h>


void G_init_locale(const char *package)
{
#ifdef HAVE_LIBINTL_H
	char localedir[4096];
	char *gisbase;

	gisbase = getenv("GISBASE");
	if (!gisbase || !*gisbase)
		return;

	sprintf(localedir, "%s/locale", gisbase);

	setlocale(LC_MESSAGES, "");
	bindtextdomain(package, localedir);
	textdomain(package);
#endif
}

char *
G_gettext(const char *package, const char *msgid)
{
	
	static char      now_bound[4096] = "";


        if (strncmp(now_bound,package,strlen(package)))
        {
                sprintf(now_bound, "%s", package);

		bindtextdomain(package, LOCALEDIR);


        }

return dgettext(package,msgid);

}


