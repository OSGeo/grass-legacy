#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "ps_map.h"

int input (char *buf)
{
    if (!fgets(buf,100,stdin)) exit(0);
    /* G_strip (buf); */ /*replaced by G_squeeze to remove \n, RB Jan 2000 */ 
    G_squeeze (buf);
    if (strcmp (buf,"exit")==0) exit(0);

    return 0;
}
