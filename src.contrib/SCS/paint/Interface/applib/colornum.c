#include "interface.h"

Pcolornum (red, grn, blu)
    float red, grn, blu;
{
    P__opcode (COLORNUM) ;
    P__sendf ((double) red);
    P__sendf ((double) grn);
    P__sendf ((double) blu);
    return P__geti ();
}
