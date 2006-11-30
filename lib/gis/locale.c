/**
 * \file locale.c
 *
 * \brief Functions to handle locale.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * \author GRASS GIS Development Team
 *
 * \date 2004-2006
 */

#include <grass/config.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <grass/glocale.h>


#if defined(HAVE_LIBINTL_H) && defined(USE_NLS)
static char *
locale_dir(void)
{
	static char localedir[4096];
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
#endif


/**
 * \fn char *G_gettext(const char *package, const char *msgid)
 *
 * \brief Gets localized text.
 *
 * \param[in] package
 * \param[in] msgid
 * \retval char * Pointer to string
 */

char *
G_gettext(const char *package, const char *msgid)
{
#if defined(HAVE_LIBINTL_H) && defined(USE_NLS)
	static char now_bound[4096];
	static int initialized;

	if (!initialized)
	{
		setlocale(LC_CTYPE, "");
		setlocale(LC_MESSAGES, "");
		initialized = 1;
	}

	if (strcmp(now_bound, package) != 0)
	{
		strcpy(now_bound, package);
		bindtextdomain(package, locale_dir());
	}

	return dgettext(package, msgid);
#else
	return (char *) msgid;
#endif
}

