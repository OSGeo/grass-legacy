#include "interface.h"

double 
Phres (void)
{
    double P__getf();

    P__opcode (HRES);
    return P__getf ();
}
