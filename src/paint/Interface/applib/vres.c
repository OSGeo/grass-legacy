#include "interface.h"
double 
Pvres (void)
{
    double P__getf();

    P__opcode (VRES);
    return P__getf ();
}
