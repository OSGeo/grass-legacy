/*
****************************************************************************
*
* MODULE:       v5d_config.c
* AUTHOR(S):    Beverly Wallace - beverly.t.wallace@lmco.com
* PURPOSE:      To dynamically create a config.h file for Vis5d.
*               The Vis5d code expects to find ../config.h.
* COPYRIGHT:    (C) 2001 by Lockheed Martin Space Systems, Sunnyvale, CA, USA 
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>

#include "gis.h"
#include "config.h"  /* Grass's config.h, used to make Vis5d's config.h */

extern int G_is_little_endian (void);


int 
main (int argc, char *argv[])
{
	/* Open the config.h file for writing, exit on error */
	FILE *fp;
	fp = fopen ("./config.h", "w");
	if (fp == NULL)
		G_fatal_error ("Can't open ./config.h for writing");

	/* Write to the file */
	fprintf (fp, "/* Config file for use by Vis5d. */\n\n");

	/* Test for HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_TYPES_H
	fprintf (fp, "#define HAVE_SYS_TYPES_H\n");
	fprintf (fp, "#define HAVE_SYS_STAT_H\n");
	fprintf (fp, "#define HAVE_FCNTL_H\n");
#endif

	/* Test for big/little endian */
	if (G_is_little_endian ()) 
		fprintf (fp, "/* #define WORDS_BIGENDIAN 1 */\n");
	else 
		fprintf (fp, "#define WORDS_BIGENDIAN 1\n");

	/* Close the file */
	fclose (fp);
	return 0;
}
