#include "gis.h"
char *
G_unctrl(c)
    unsigned char c;
{
    static char buf[20];

    if (c < ' ')
	sprintf (buf, "ctrl-%c", c|0100);
    else if (c < 0177)
	sprintf(buf, "%c", c);
    else if (c == 0177)
	sprintf (buf, "DEL/RUB");
    else
	sprintf (buf, "Mctrl-%c", (c&77)|0100);
    return buf;
}
