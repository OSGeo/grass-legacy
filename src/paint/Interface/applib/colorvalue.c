#include "interface.h"

Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    double P__getf() ;

    P__opcode (COLORVALUE) ;
    P__sendi (n);
    *red = P__getf();
    *grn = P__getf();
    *blu = P__getf();
}
