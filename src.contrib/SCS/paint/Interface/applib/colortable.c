#include "interface.h"
#include <stdio.h>

Pcolortable (red, grn, blu, table, n)
    unsigned char *red, *grn, *blu;
    unsigned char *table;
{
    int i;

    while (n > 0)
    {
	i = n;
	if (i > TABLE_SIZE)
	    i = TABLE_SIZE;
	P__opcode (COLORTABLE);
	P__sendi (i);
	P__send (red, i);
	P__send (grn, i);
	P__send (blu, i);
	P__get (table, i);

	red += i;
	grn += i;
	blu += i;
	table += i;
	n -= i;
    }
}
