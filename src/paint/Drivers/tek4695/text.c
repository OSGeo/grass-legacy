#include "P.h"
Ptext (s) unsigned char *s;
{
    while (*s >= ' ' && *s < 0177)
	Poutc (*s++);
    Poutc('\n');
}
