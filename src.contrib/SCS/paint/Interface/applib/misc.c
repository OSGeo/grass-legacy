#include "interface.h"

Pblockspace()
{
    P__opcode (BLOCKSPACE);
    return P__geti ();
}

Pblocksize()
{
    P__opcode (BLOCKSIZE);
    return P__geti ();
}

Pnblocks()
{
    P__opcode (NBLOCKS);
    return P__geti ();
}

Ptextspace()
{
    P__opcode (TEXTSPACE);
    return P__geti ();
}

Ptextfudge()
{
    P__opcode (TEXTFUDGE);
    return P__geti ();
}

double
Ptextscale()
{
    double P__getf();

    P__opcode (TEXTSCALE);
    return P__getf ();
}
