#include <config.h>

#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#define _(str) gettext(str)
char * libgrass_gettext(const char *, const char *);
#else
#define _(str) str

#endif

