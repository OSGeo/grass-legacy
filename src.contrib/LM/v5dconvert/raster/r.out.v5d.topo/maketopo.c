/* maketopo.c */
/*
 * Vis5D system for visualizing five dimensional gridded data sets.
 * Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
 * Dave Santek, and Andre Battaiola.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * As a special exception to the terms of the GNU General Public
 * License, you are permitted to link Vis5D with (and distribute the
 * resulting source and executables) the LUI library (copyright by
 * Stellar Computer Inc. and licensed for distribution with Vis5D),
 * the McIDAS library, and/or the NetCDF library, where those
 * libraries are governed by the terms of their own licenses.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "../config.h"


/* Utility to make new topography files for Vis5d.

  compile with:  cc maketopo.c -o maketopo


  Basically, a topography is just a 2-dimensional array of elevations values.
  To make your own topography file for Vis5d, follow these steps:

  1. Set the ROWS and COLUMNS to indicate how large the topography matrix is.

  2. Set the WEST, EAST, NORTH, and SOUTH values to indicate what region
     of earth your topography spans.  These numbers are in degrees of
     latitude  (+ is north, - is south ) and degrees of longitude
      (+ is west, - is east ).

  3. Specify a FILENAME for the topography file.

  4. Put your topography values into the Topo matrix.  Each value is a
     floating point value in meters above sea level.  Positive values are
     above sea level and negative values are below.  All values must be
     in the interval [-8100 , 8100].

     Row [0] corresponds to the north edge of the topography.
     Row [ROWS-1] corresponds to the south edge.
     Column [0] corresponds to the west edge.
     Column [COLUMNS-1] corresponds to the east edge.

  5. [optional]
     Put land/water flags into the Water matrix.  The Water matrix is
     a 2-D array the same size as the Topo matrix.  If Water[row][column]
     is non-zero, then the topography will be blue at that location,
     otherwise the topography is considered to be land.

     The purpose of the Water matrix is to allow you to have water at
     elevations above sea level and have land at elevations below sea level.

*/




#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif


/* Changes made for use with Grass 
 * by Bev Wallace (beverly.t.wallace@lmco.com), May 2001
 *	Separated the main program into functions.
 *	Removed global variables and region defines.
 *	Updated to ANSI C.
 *	Used return instead of exit upon error.
 *	Used fprintf instead of printf.
 *	Added optional DEBUG statements.
 *      Make with gmake5.
 */

/* Uncomment for debug messages - Bev Wallace */
/* #define DEBUG */



#include <stdio.h>
#include <float.h>
#include <malloc.h>	/* Added by Bev Wallace */

#include "binio.h"


int topo_data (int fd, float *Topo, int array_size)
{
   int i;
   short int *val;
   int Water = 1;

   val = (short int *) malloc (sizeof(short int) * array_size);
   if (!val) {
        fprintf (stdout, "maketopo:  Error allocating val data, size=%d\n", 
		array_size);
        return 0;
   }

   /* Convert each elevation to a short integer, multiply it by two and */
   /* add 1 if the Water flag is set */
   for (i=0; i<array_size; i++) {
	if (Topo[i] == FLT_MAX) {
		val[i] = 0;
		if (Water)
			val[i] += 1;
	}
	else {
		val[i] = (short int) (Topo[i] * 2);
		if (Water && Topo[i] <= 0.0)
			val[i] += 1;
	}
   }

   /* write the topo data */
   write_int2_array (fd, val, array_size);

   return 1;
}


int topo_init (int rows, int cols,
	float north, float south, float east, float west,
	char *filename)
{
	int fd;
	mode_t mask = 0666;

#ifdef DEBUG
	fprintf (stdout, "In topo_init\n");
	fprintf (stdout, " north=%f, south=%f\n", north, south);
	fprintf (stdout, " east=%f, west=%f\n", east, west);
	fprintf (stdout, "filename=%s\n", filename);
#endif /* DEBUG */

	/*** Now write the file ***/
	fd = open (filename, O_WRONLY|O_CREAT, mask );
	if (fd == -1) {
		/* error */
		fprintf (stdout, "Error opening file %s\n", filename);
		return fd;
	}

	/* Write the header */ 
	write_bytes (fd, "TOPO2", 40); /* TOPO = int data, TOPO2 = float data */
	write_float4 (fd, west);
	write_float4 (fd, east);
	write_float4 (fd, north);
	write_float4 (fd, south);
	write_int4 (fd, rows);
	write_int4 (fd, cols);
 
	return fd;
}


int topo_final (int fd)
{
	close (fd);
	return 1;
}

