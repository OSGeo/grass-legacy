#include <stdio.h>
#include "interface.h"

Plock()
{
    P__opcode (LOCK);
    switch (P__geti())
    {
    case 0: return;
    case 1: fprintf (stderr, "Printer already in use\n"); break;
    default: fprintf (stderr, "Unable to create lock file\n"); break;
    }
    exit(0);
}
