#include <stdlib.h>
#include <stdio.h>
#include "proto.h"

int list_painters()
{
    fprintf (stdout,"\nAvailable PAINTERS\n\n");
    system ("p.select -l");

  return 0;
}
