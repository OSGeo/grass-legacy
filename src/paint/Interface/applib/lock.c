#include <stdio.h>
#include "interface.h"

int Plock (void)
{
    P__opcode (LOCK);
    switch (P__geti())
    {
    case 0: return 1;
    case 1: fprintf (stderr, "Printer already in use\n"); break;
    default: fprintf (stderr, "Unable to create lock file\n"); break;
    }
    exit(0);
}
