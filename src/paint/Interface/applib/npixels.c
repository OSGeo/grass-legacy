#include "interface.h"

int Pnpixels (int *rows, int *cols)
{
    P__opcode (NPIXELS);
    *rows = P__geti ();
    *cols = P__geti ();

    return 0;
}
