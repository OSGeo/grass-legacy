#include "interface.h"

int 
Pcolormultipliers (int *red, int *grn, int *blu)
{
    P__opcode (COLORMULT) ;
    *red = P__geti();
    *grn = P__geti();
    *blu = P__geti();

    return 0;
}
