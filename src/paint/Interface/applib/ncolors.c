#include "interface.h"

int 
Pncolors (void)
{
    P__opcode (NCOLORS);
    return P__geti ();
}
