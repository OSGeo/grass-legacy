/* makemap.c */
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


/*
 * Utility program to make your own map outline files.
 *
 * A map outline file consists of a number of polylines.  A polyline
 * is a sequence of connected line segments.  The end points (vertices)
 * of the lines are defined in degrees of latitude and longitude.
 *
 * Compile with:   make -f makemap.m
 * 
 */


/* Changes made for use with Grass 
 * by Bev Wallace (beverly.t.wallace@lmco.com), May 2001
 *	Updated to ANSI C.
 *	Used return instead of exit upon error.
 *	Used fprintf instead of printf.
 *	Added optional DEBUG statements.
 *	Added arguments to initialize() for use with any MapLine.
 *	Rounded the lat/lon values.
 *      Make with gmake5.
 */

/* Uncomment for debug messages - Bev Wallace */
/* #define DEBUG */

/* Declare functions - Bev Wallace */
extern int vertex (float lat, float lon);
extern int end_line (void);
extern int done (char *filename);


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

#include <binio.h>


int example_main (int argc, char *argv[])
{
   /****** Replace this example code with your own! *****/

   /* this example writes a mapfile which spells out "ABC" like this:

                                               |
                                               |    L
         2*       7*-*8      12*-----*11       +50  a
         / \       |  \        |               |    t
        /   \      |   \       |               |    i
      1*-----*3   6*----*9     |               +35  t
       |     |     |    |      |               |    u
       |     |     |    |      |               |    d
      0*     *4   5*----*10  13*-----*14       +20  e
                                               |
  -----+-----+-----+----+------+-----+---------+---
      130   120   110  100     90    80        |0
                    Longitude                  |


   The asterisks are the vertices.  The polylines are defined by:
      0,1,2,3,4
      1,3
      5,6,7,8,9,10,5
      6,9
      11,12,13,14

   Below is the code to make this map.  All you have to do is call
   these functions:

       vertex( lat, lon )   - to specify the next vertex in lat/lon
       end_line()           - to signal the end of a polyline
       done( filename )     - to write the named map file


   If you use this example unchanged then you can view the map with:
       vis5d LAMPS.v5d -map util/OUTLABC
   */

   /* Letter A */
   vertex( 20.0, 130.0 );  /* vertex 0 */
   vertex( 35.0, 130.0 );  /* vertex 1 */
   vertex( 50.0, 125.0 );  /* vertex 2 */
   vertex( 35.0, 120.0 );  /* vertex 3 */
   vertex( 20.0, 120.0 );  /* vertex 4 */
   end_line();
   vertex( 35.0, 130.0 );  /* vertex 1 */
   vertex( 35.0, 120.0 );  /* vertex 3 */
   end_line();

   /* Letter B */
   vertex( 20.0, 110.0 );  /* vertex 5 */
   vertex( 35.0, 110.0 );
   vertex( 50.0, 110.0 );
   vertex( 50.0, 104.0 );
   vertex( 35.0, 100.0 );
   vertex( 20.0, 100.0 );  /* vertex 10 */
   vertex( 20.0, 110.0 );  /* vertex 5 */
   end_line();
   vertex( 35.0, 110.0 );
   vertex( 35.0, 100.0 );
   end_line();

   /* Letter C */
   vertex( 50.0, 80.0 );
   vertex( 50.0, 90.0 );
   vertex( 20.0, 90.0 );
   vertex( 20.0, 80.0 );
   end_line();

   /* Write the file! */
   done( "OUTLABC" );

   return (0);
}




/**********************************************************************/
/***        No changes beyond this point should be necessary.       ***/
/**********************************************************************/



struct polyline {
        int        minlat;
        int        maxlat;
        int        minlon;
        int        maxlon;
        int        start;
        int        len;
};


#define MAXLINES 100000
struct polyline MapLine[MAXLINES];
int NumLines = 0;


/* Changed structure name from vertex to coord - Bev Wallace */
struct coord {
        int       lat, lon;
};


#define MAXVERTS 100000
struct coord VertexList[MAXVERTS];
int NumVertices = 0;


/* Modified to initialize any MapLine, not just the first - Bev Wallace */
int initialize (int index, int start)
{
   MapLine[index].minlat =  10000000;
   MapLine[index].maxlat = -10000000;
   MapLine[index].minlon =  10000000;
   MapLine[index].maxlon = -10000000;
   MapLine[index].start = start;
   MapLine[index].len = 0;

   return 1;
}



int vertex (float lat, float lon)
{
   /* Don't need init_flag, use NumVertices to initialize - Bev Wallace */
   /* static int init_flag = 1; */
   if (NumVertices == 0) {
	initialize (0, 0);
   }
   if (NumVertices>=MAXVERTS) {
      fprintf (stderr, "Out of space for map vertices!\n");
      return 0;
   }
   else {     
      int ilat = (int) (lat * 10000.0 + .5);  /* Round - Bev Wallace */
      int ilon = (int) (lon * 10000.0 + .5);  /* Round - Bev Wallace */

#ifdef DEBUG
      fprintf (stdout, "lat=%f ilat=%d  lon=%f ilon=%d\n", 
          lat, ilat, lon, ilon);
#endif

      VertexList[NumVertices].lat = ilat;
      VertexList[NumVertices].lon = ilon;
      NumVertices++;
      MapLine[NumLines].len++;
      if (ilat > MapLine[NumLines].maxlat)   MapLine[NumLines].maxlat = ilat;
      if (ilat < MapLine[NumLines].minlat)   MapLine[NumLines].minlat = ilat;
      if (ilon > MapLine[NumLines].maxlon)   MapLine[NumLines].maxlon = ilon;
      if (ilon < MapLine[NumLines].minlon)   MapLine[NumLines].minlon = ilon;
   }
   return 1;
}



int end_line (void)
{
   if (NumVertices==0 || MapLine[NumLines].len==0) {
      fprintf (stderr, "Error:  must call vertex() before end_line()!\n");
      return 0;
   }

#ifdef DEBUG
      fprintf (stdout, "End Line\n");
#endif

   MapLine[NumLines].len *= 2;
   NumLines++;
   if (NumLines>=MAXLINES) {
      fprintf (stderr, "Error: out of space for lines!\n");
      return 0;
   }
   initialize (NumLines, NumVertices);
   return 1;
}



int done (char *filename)
{
   int f;
   mode_t mask;
   int i;

   /* Convert MapLine[].start values from index into VertexList to
    * absolute file positions (in 4-byte units).
    */
   for (i=0;i<NumLines;i++) {
      int index, pos;
      index = MapLine[i].start;
      pos = 6*NumLines + index*2 + 1;
      MapLine[i].start = pos;
   }


   mask = 0666;
   f = open( filename, O_WRONLY | O_CREAT | O_TRUNC, mask );
   if (f<0) {
      fprintf (stderr, "Error:  unable to open %s for writing\n", filename );
      return (0);
   }

   write_int4( f, NumLines );

   if (write_int4_array( f, (int *) MapLine, 6*NumLines ) < 6*NumLines) {
      fprintf (stderr, "Error:  bad file write (disk full?)\n");
      return (0);
   }

   if (write_int4_array( f, (int *) VertexList, 2*NumVertices ) < 2*NumVertices) {
      fprintf (stderr, "Error:  bad file write (disk full?)\n");
      return (0);
   }

   close( f );
   return (1);
}


