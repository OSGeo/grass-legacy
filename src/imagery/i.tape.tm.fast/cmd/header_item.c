#include "tape.h"
/* copy from tape buffer into 'buf' */

char *
header_item (start, end)
{
    static char buf[100];
    register char *b;

    b = buf;
    while (start <= end)
	*b++ = tape.headbuf[start++];
    *b = 0;
    return (buf);
}
