#include "interface.h"

Pcolormultipliers (red, grn, blu)
    int *red, *grn, *blu;
{
    P__opcode (COLORMULT) ;
    *red = P__geti();
    *grn = P__geti();
    *blu = P__geti();
}
