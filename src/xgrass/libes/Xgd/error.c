#include "xgrass_dlib.h"

/*
 ***************************************************************************
 * XgdError - Print the error message then die.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdError(s)
char *s;
#else
XgdError(char *s)
#endif
{
        fprintf(stderr, "XGRASS DISPLAY LIB ERROR: %s\n",s);
        exit(1);
}
