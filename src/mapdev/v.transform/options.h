/*
****************************************************************************
*
* MODULE:       v.transform
* AUTHOR(S):    See other files as well...
*               Eric G. Miller <egm2@jps.net>
* PURPOSE:      To transform a vector layer's coordinates via a set of tie
*               points.
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include	"gis.h"

#define MAPIN	1
#define MAPOUT	2
#define COORD	3
#define VERBOSE	4

/*  We want main.c to access the following, but not command.c  */

#ifdef  MAIN

	struct Command_keys com_keys[] =
	{
	    {"mapin",  MAPIN},
	    {"in",     MAPIN},
	    {"mapout", MAPOUT},
	    {"out",    MAPOUT},
	    {"coordfile", COORD},
	    {"coord",  COORD},
	    {"coors",  COORD},
	    {"coor",   COORD},
	    {"c",      COORD},
	    {"verbose", VERBOSE},
	    {"v",      VERBOSE},
	    {NULL,     0}
	} ;

#endif

