/****************************************************************
* Purpose:  This set of functions checks for GRASS_VERBOSE
*           environment variable
*
* Author:   Jachym Cepicky <jachym.cepicky centrum cz> 
*
* Copyright: (C) 2006 by the GRASS Development Team
*
*           This program is free software under the GNU General Public
*           License (>=v2). Read the file COPYING that comes with GRASS
*           for details.
*           
* see also:
*   G_percent
*   G_message()
*   G_warning()
******************************************************************/
#include <grass/config.h>
#include <stdlib.h>

#define MAXLEVEL 2 
#define MINLEVEL 0


static int verbose; /* current verbosity level */


/*!
 * \brief get current verbosity level
 *
 * Currently, there are 3 levels of verbosity:
 * \param 0 - module should print nothing but errors and warnings (G_fatal_error, G_warning)
 * \param 1 - module will print progres information (G_percent)
 * \param 2 - module will print all messages (G_message)
 */
int G_verbose (void)

{
    char *verstr; /* string for GRASS_VERBOSE content */

    /* verbose not defined -> get it from env. */ 
    if ( !verbose ) {

        if ( (verstr = getenv ( "GRASS_VERBOSE" )) ) {
            if (verbose = atoi ( verstr ))
                ;
        }
        else
            verbose = MINLEVEL;
    }
    return verbose;
}

/*!
 * \brief get max verbosity level
 *
 * returns max verbosity level
 */
int G_verbose_max (void)
{
    return MAXLEVEL;
}

/*!
 * \brief get min verbosity level
 *
 * returns min verbosity level
 */
int G_verbose_min (void)
{
    return MINLEVEL;
}

