/******************************************************************************
 * $Id$
 *
 * Project:  PROJ.4
 * Purpose:  Built in datum list.
 * Author:   Frank Warmerdam, warmerda@home.com
 *
 ******************************************************************************
 * Copyright (c) 2000, Frank Warmerdam
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 *
 * $Log$
 * Revision 1.4  2003-03-13 09:48:31  paul
 * Tidied and improved datum and datum transformation handling
 * Fixed various bugs in g.setproj
 * Improved error reporting in pj_do_proj()
 * Changed various Gmakefiles to link $(GPROJLIB) before $(GISLIB)
 *
 * Revision 1.3  2003/01/24 12:09:43  paul
 * Add support for Ireland 1965 Datum
 * Fix missing underscores in pj_ellps.c and pj_datums.c
 *
 * Revision 1.2  2002/05/29 10:21:22  markus
 * added Krovak and Krovakgis projection (Krovak already present in latest PROJ4 CVS version, added Hermannskogel datum for Krovakgis
 *
 * Revision 1.1  2002/04/20 19:13:44  roger
 * Updating Proj lib to 4.4.5, and adding two new functions for datum conversions
 *
 */

#define PJ_DATUMS__

#include "projects.h"

/* 
 * The ellipse code must match one from pj_ellps.c.  The datum id should
 * be kept to 12 characters or less if possible.  Use the official OGC 
 * datum name for the comments if available. 
 */

struct PJ_DATUMS pj_datums[] = {
/* id       definition                               ellipse  comments */
/* --       ----------                               -------  -------- */
"WGS84",    "towgs84=0,0,0", 		             "WGS84", "",
"GGRS87",   "towgs84=-199.87,74.79,246.62",          "GRS80", 
				"Greek_Geodetic_Reference_System_1987",
"NAD83",    "towgs84=0,0,0",                         "GRS80", 
				"North_American_Datum_1983",
"NAD27",    "nadgrids=conus,ntv1_can.dat",           "clrk66", 
				"North_American_Datum_1927",
"potsdam",  "towgs84=606.0,23.0,413.0",  "bessel",  "Potsdam Rauenberg 1950 DHDN",
"carthage",  "towgs84=-263.0,6.0,431.0",  "clark80",  "Carthage 1934 Tunisia",
"hermannskogel", "towgs84=653.0,-212.0,449.0",  "bessel",  "Hermannskogel",
"ire65",  "towgs84=482.530,-130.596,564.557,-1.042,-0.214,-0.631,8.15",  "mod_airy",  "Ireland 1965",
NULL,       NULL,                                    NULL,    NULL 
};
