#include "interface.h"
int 
Pclose (void)
{
    P__opcode (FLUSH); /* added by Dave Duran at NPS */
    P__opcode (CLOSE);

    return 0;
}
