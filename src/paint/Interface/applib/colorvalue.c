#include "interface.h"

int 
Pcolorvalue (int n, float *red, float *grn, float *blu)
{
    double P__getf() ;

    P__opcode (COLORVALUE) ;
    P__sendi (n);
    *red = P__getf();
    *grn = P__getf();
    *blu = P__getf();

    return 0;
}
