/**
 * \file verbose.c
 *
 * \brief Functions to check for GRASS_VERBOSE environment variable.
 *
 * see also:
 * G_percent()
 * G_message()
 * G_warning()
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
 * \author Jachym Cepicky - jachym.cepicky at centrum cz
 *
 * \date 2006
 */

#include <grass/config.h>
#include <stdlib.h>

#define MAXLEVEL 3
#define STDLEVEL 2
#define MINLEVEL 0


static int verbose = -1; /* current verbosity level */


/**
 * \fn int G_verbose (void)
 *
 * \brief Get current verbosity level.
 *
 * "--v" requires if(G_verbose() == G_verbose_max()) tests.
 *
 * Currently, there are 3 levels of verbosity.
 *
 * \return 0 - module should print nothing but errors and warnings (G_fatal_error, G_warning)
 * \return 1 - module will print progress information (G_percent)
 * \return 2 - module will print all messages (G_message)
 * \return 3 - module will be very verbose. Triggered by "--v".
 */

int G_verbose (void)

{
    char *verstr; /* string for GRASS_VERBOSE content */

    /* verbose not defined -> get it from env. */ 
    if ( verbose < 0 ) {

        if ( (verstr = getenv ( "GRASS_VERBOSE" )) ) {
            if ((verbose = atoi ( verstr )))
                ;
        }
        else
            verbose = STDLEVEL;
    }
    return verbose;
}


/**
 * \fn int G_verbose_max (void)
 *
 * \brief Get max verbosity level.
 *
 * \return max verbosity level.
 */

int G_verbose_max (void)
{
    return MAXLEVEL;
}


/**
 * \fn int G_verbose_std (void)
 *
 * \brief Get standard verbosity level.
 *
 * \return standard verbosity level
 */

int G_verbose_std (void)
{
    return STDLEVEL;
}


/**
 * \fn int G_verbose_min (void)
 *
 * \brief Get min verbosity level.
 *
 * \return min verbosity level
 */

int G_verbose_min (void)
{
    return MINLEVEL;
}
