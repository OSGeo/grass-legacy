/**
 * \file endian.c
 *
 * \brief Functions to determine architecture endian.
 *
 * This endian test was taken from ./src.contrib/GMSL/NVIZ2.2/TOGL/apps/image.c.
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
 * \author Markus Neteler
 *
 * \date 2001-2006
 */


/**
 * \fn int G_is_little_endian (void)
 *
 * \brief Tests for little ENDIAN.
 *
 * Test if machine is little or big endian.
 *
 * \return 1 little endian
 * \return 0 big endian
 */

int G_is_little_endian (void)    
{
    union {
        int testWord;
        char testByte[sizeof(int)];
    } endianTest;

    endianTest.testWord = 1;
    
    if (endianTest.testByte[0] == 1)
        return 1; /* true: little endian */

    return 0; /* false: big endian */
}
