/**
 * \file unctrl.c
 *
 * \brief Handles control characters.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * \author GRASS GIS Development Team
 *
 * \date 1999-2006
 */

#include <stdio.h>
#include <grass/gis.h>


/**
 * \fn char *G_unctrl (int c)
 *
 * \brief Printable version of control character.
 *
 * This routine returns a pointer to a string which contains an
 * English-like representation for the character <b>c</b>. This is useful for
 * nonprinting characters, such as control characters. Control characters are
 * represented by ctrl-C, e.g., control A is represented by ctrl-A. 0177 is
 * represented by DEL/RUB. Normal characters remain unchanged.
 *
 * \param[in] int c
 * \retval char * pointer to string containing English-like representation for character <b>c</b>
 */

char *G_unctrl (int c)
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
