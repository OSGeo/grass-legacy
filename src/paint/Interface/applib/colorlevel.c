#include "interface.h"

int 
Pcolorlevels (int *red, int *grn, int *blu)
{
    P__opcode (COLORLEVELS) ;
    *red = P__geti();
    *grn = P__geti();
    *blu = P__geti();

    return 0;
}
