
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
libgrass_gettext(const char *package, const char *msgid)
{

#ifdef HAVE_LIBINTL_H
        static int      already_bound = 0;

        if (!already_bound)
        {
                already_bound = 1;

		bindtextdomain(package, LOCALEDIR);


        }

return dgettext(package,msgid);
#else
return msgid;       
#endif
}


