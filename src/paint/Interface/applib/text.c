#include "interface.h"

int Ptext (char *s)
{
    P__opcode (TEXT);
    P__sends(s);

    return 0;
}
