#include "interface.h"

int 
Ppictsize (int nrows, int ncols)
{
    P__opcode (PICTSIZE);
    P__sendi (nrows);
    P__sendi (ncols);
    Prle_set_cols (ncols);

    return 0;
}
