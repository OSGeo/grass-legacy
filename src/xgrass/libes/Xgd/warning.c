#include "xgrass_dlib.h"

/*
 ***************************************************************************
 * XgdWarning - fire off the missles and hope for the best
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdWarning(s)
char *s;
#else
XgdWarning(char *s)
#endif
{
        fprintf(stderr, "XGRASS DISPLAY LIB WARNING: %s\n",s);
}
