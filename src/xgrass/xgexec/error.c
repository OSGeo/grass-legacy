/**********************************************************************
   error.c      - report any error (fatal, warning, etc.)
 *********************************************************************/

#include "xgrass.h"

FatalError(s,t)
    char *s, *t;
{
    fprintf(stderr,"%s: FATAL ERROR <%s>\n\t%s\n",_XG_Global.progName,s,t);
    XFlush(_XG_Global.display);
    exit(1);
}

void
Interrupt()
{
    fprintf(stderr,"%s: FATAL ERROR <%s>\n\t%s\n",_XG_Global.progName,
         "Caught signal causing death","Later Dude!!");
    XFlush(_XG_Global.display);
    exit(0);
}
