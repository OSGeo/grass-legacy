#include "interface.h"

Ppictsize(nrows,ncols)
{
    P__opcode (PICTSIZE);
    P__sendi (nrows);
    P__sendi (ncols);
    Prle_set_cols (ncols);
}
