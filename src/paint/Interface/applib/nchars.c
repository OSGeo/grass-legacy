#include "interface.h"

int 
Pnchars (void)
{
    P__opcode (NCHARS);
    return P__geti ();
}
