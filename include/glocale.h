#include <config.h>

#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#define _(str) libgrass_gettext(str)

char * libgrass_gettext(const char *);

#else
#define _(str) str
#endif

