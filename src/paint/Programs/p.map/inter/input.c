#include <stdio.h>
#include <string.h>
#include "gis.h"

int input (char *buf)
{
    if(!fgets(buf,80,stdin)) exit(0);
    G_strip (buf);
    if (strcmp (buf,"exit")==0) exit(0);

  return 0;
}
