#include "gis.h"

/*!
 * \brief printable version of control character
 *
 * This routine returns a pointer to a string which contains an
 * English-like representation for the character <b>c.</b> This is useful for
 * nonprinting characters, such as control characters. Control characters are
 * represented by ctrl-C, e.g., control A is represented by ctrl-A. 0177 is
 * represented by DEL/RUB. Normal characters remain unchanged.
 *
 *  \param c
 *  \return char * 
 */

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
