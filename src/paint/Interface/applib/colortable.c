#include "interface.h"
#include <stdio.h>

int Pcolortable (
    unsigned char *red, unsigned char *grn, unsigned char *blu,
    unsigned char *table, int n)
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

    return 0;
}
