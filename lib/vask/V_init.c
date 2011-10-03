
/**
 * \file V_init.c
 *
 * \brief Interactive initialization functions.
 *
 * (C) 1999-2009 by the GRASS Development Team 
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * \author GRASS GIS Development Team
 *
 * \date 1999-2009
 */

#include <grass/config.h>
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/vask.h>


/**
 * \fn int V_init ()
 *
 * \brief Initialize curses and prepare screen.
 *
 * \return always returns 0
 */

void V_init(void)
{
    static int first = 1;

    G_clear_screen();		/* this is a kludge - xterm has problems
				 * it shows what was on the screen after
				 * endwin is called in V_exit()
				 */
    if (first) {
	initscr();		/* initialize curses and tty */
	first = 0;
    }

    /* the order of these 3 calls is important for
     * Mips' braindead implementation of curses  */
    noecho();
    nonl();
    raw();

    clear();
    refresh();
#ifdef HAVE_KEYPAD
    keypad(stdscr, 1);
#endif
}
