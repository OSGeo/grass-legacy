#include "interface.h"
Pclose()
{
    P__opcode (FLUSH); /* added by Dave Duran at NPS */
    P__opcode (CLOSE);
}
