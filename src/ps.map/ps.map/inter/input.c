#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "ps_map.h"

int input (char *buf)
{
    if (!fgets(buf,100,stdin)) exit(0);
  /******* remove CR because of fgets   Werner.Droege@mailbox.tu-dresden.de 
   * required for Solaris */
    buf[strlen(buf)-1]='\0';
    
    G_strip (buf);
    if (strcmp (buf,"exit")==0) exit(0);

    return 0;
}
