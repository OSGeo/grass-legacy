#include "interface.h"

int Pflush (void)
{
    P__opcode (FLUSH);

    return 0;
}
