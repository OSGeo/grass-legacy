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
 * Revision 1.7  2000-11-24 08:33:47  justin
 * Can't define COPYING with cat in Gmakefile - see main.c for proper definition
 *
 * Revision 1.6  2000/11/08 20:32:54  andreas
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
#define COPYING "Copyright and License Statement\n\
\n\
The Geographic Resources Analysis and Support System (GRASS)\n\
Geographic Information System (GIS) is Copyright by the\n\
GRASS Development Team headquartered at Baylor University,\n\
in Waco, Texas.\n\
\n\
This program is free software; you can redistribute it and/or modify it\n\
under the terms of the GNU General Public License as published by the\n\
Free Software Foundation; either version 2 of the License, or (at your\n\
option) any later version.\n\
\n\
Parts of GRASS are not copyright by the GRASS development team.\n\
The original authors hold the copyrights and you have to abide \n\
to their licensing terms where noted.\n\
(Keep in mind that code linking into GRASS can only be distributed\n\
if compatible with the GPL.)\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License (GPL) for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program; if not, write to the\n\
  Free Software Foundation, Inc.,\n\
  59 Temple Place - Suite 330,\n\
  Boston, MA  02111-1307, USA.\n\
\n\
Questions regarding GRASS GIS should be directed to the\n\
GRASS Development Team at the following address:\n\
\n\
European Headquarters\n\
 GRASS Development Team\n\
 Institute of Physical Geography-Landscape Ecology\n\
 University of Hannover\n\
 Schneiderberg 50\n\
 30167 Hannover, Germany\n\
 email: neteler@geog.uni-hannover.de\n\
\n\
United States Headquarters\n\
 GRASS Development Team\n\
 Center for Applied Geographic and Spatial Research\n\
 Baylor University\n\
 P.O. Box 97351\n\
 Waco, Texas  76798-7351\n\
 Email:     grass@baylor.edu\n\
\n\
Internet:  http://www.baylor.edu/~grass\n\
           http://www.geog.uni-hannover.de/grass\n"
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
