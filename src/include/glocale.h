#include "config.h"

extern void G_init_locale(void);
extern char * G_gettext(const char *, const char *);

#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#define _(str) G_gettext(PACKAGE,(str))
#else
#define _(str) (str)
#endif

