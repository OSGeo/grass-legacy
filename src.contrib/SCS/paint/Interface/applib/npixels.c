#include "interface.h"

Pnpixels(rows,cols)
    int *rows, *cols;
{
    P__opcode (NPIXELS);
    *rows = P__geti ();
    *cols = P__geti ();
}
