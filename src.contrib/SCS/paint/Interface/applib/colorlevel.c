#include "interface.h"

Pcolorlevels (red, grn, blu)
    int *red, *grn, *blu;
{
    P__opcode (COLORLEVELS) ;
    *red = P__geti();
    *grn = P__geti();
    *blu = P__geti();
}
