#include "P.h"

Prle (buf, n)
    unsigned char *buf;
{
    unsigned char i;

    Poutc (RLE);
    Pout (buf, n + n);
    if (padding > 0)
    {
	i = padding;
	Poutc (i);
	Poutc (WHITE);
    }
}
