#include <config.h>

#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#define _(str) gettext(str)
#else
#define _(str) str
#endif

