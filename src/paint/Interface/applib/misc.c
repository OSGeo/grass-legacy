#include "interface.h"

int 
Pblockspace (void)
{
    P__opcode (BLOCKSPACE);
    return P__geti ();
}

int 
Pblocksize (void)
{
    P__opcode (BLOCKSIZE);
    return P__geti ();
}

int 
Pnblocks (void)
{
    P__opcode (NBLOCKS);
    return P__geti ();
}

int 
Ptextspace (void)
{
    P__opcode (TEXTSPACE);
    return P__geti ();
}

int 
Ptextfudge (void)
{
    P__opcode (TEXTFUDGE);
    return P__geti ();
}

double 
Ptextscale (void)
{
    double P__getf();

    P__opcode (TEXTSCALE);
    return P__getf ();
}
