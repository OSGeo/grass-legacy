/***************************************************************************
 * $Id$
 *
 * MODULE: 	g.version
 * AUTHOR(S):	Michael Shapiro, CERL
 *              Andreas Lange - <andreas.lange@rhein-main.de>
 * PURPOSE: 	Output GRASS version number, date and copyright message.
 *             
 * COPYRIGHT:  	(C) 2000 by the GRASS Development Team
 *
 *   	    	This program is free software under the GPL (>=v2)
 *   	    	Read the file COPYING that comes with GRASS for details.
 ****************************************************************************
 * $Log$
 * Revision 1.6  2000-11-08 20:32:54  andreas
 * added automatic input of COPYING file for copyr. msg.
 *
 */

#include <stdio.h>

#ifndef VERSION_NUMBER
#define VERSION_NUMBER "5.0"
#endif
#ifndef VERSION_DATE 
#define VERSION_DATE "2000"
#endif
#ifndef VERSION_UPDATE_PKG
#define VERSION_UPDATE_PKG "0.1"
#endif
#ifndef COPYING
#define COPYING "Copyright and License Statement

The Geographic Resources Analysis and Support System (GRASS)
Geographic Information System (GIS) is Copyright by the
GRASS Development Team headquartered at Baylor University,
in Waco, Texas.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

Parts of GRASS are not copyright by the GRASS development team.
The original authors hold the copyrights and you have to abide
to their licensing terms where noted.
(Keep in mind that code linking into GRASS can only be distributed 
if compatible with the GPL.)                               

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License (GPL) for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the
  Free Software Foundation, Inc.,
  59 Temple Place - Suite 330,
  Boston, MA  02111-1307, USA.

Questions regarding GRASS GIS should be directed to the
GRASS Development Team at the following address:

European Headquarters 
 GRASS Development Team
 Institute of Physical Geography-Landscape Ecology
 University of Hannover  
 Schneiderberg 50 
 30167 Hannover, Germany 
 email: neteler@geog.uni-hannover.de

United States Headquarters 
 GRASS Development Team
 Center for Applied Geographic and Spatial Research
 Baylor University
 P.O. Box 97351
 Waco, Texas  76798-7351
 Email:     grass@baylor.edu

Internet:  http://www.baylor.edu/~grass
           http://www.geog.uni-hannover.de/grass"
#endif

/* did'nt work in practice, though it should,
 * andreas lange 07/2000 
 * #define QUOTE(x) #x
 */

int main(int argc, char *argv[])
{
  fprintf (stdout, "GRASS %s (%s) %s\n",
	   VERSION_NUMBER, VERSION_DATE, VERSION_UPDATE_PKG );
  fprintf (stdout, "\n");
  fprintf (stdout, "%s", COPYING);
  fprintf (stdout, "\n");
  exit(0);
}
