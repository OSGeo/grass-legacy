#include "interface.h"

int 
Pcolornum (double red, double grn, double blu)
{
    P__opcode (COLORNUM) ;
    P__sendf ((double) red);
    P__sendf ((double) grn);
    P__sendf ((double) blu);
    return P__geti ();
}
