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
 * Revision 1.1  2002-04-20 19:13:44  roger
 * Updating Proj lib to 4.4.5, and adding two new functions for datum conversions
 *
 * Revision 1.2  2001/04/05 19:32:41  warmerda
 * added ntv1_can.dat to NAD27 list
 *
 * Revision 1.1  2000/07/06 23:32:27  warmerda
 * New
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
"ggrs87",   "towgs84=-199.87,74.79,246.62",          "grs80", "Greek Geodetic Reference System 1987",
"nad27",    "nadgrids=conus,ntv1 can.dat",           "clark66", "North American Datum 1927",
"wgs84",  "towgs84=0.0,0.0,0.0",  "wgs84",  "World Geodetic System 1984",
"wgs72",  "towgs84=0.0,0.0,5.0",  "wgs72",  "World Geodetic System 1972",
"nad83",  "towgs84=0.0,0.0,0.0",  "grs80",  "North American 1983",
"a-can",  "towgs84=-9.0,151.0,185.0",  "clark66",  "Alaska and Canada NAD27",
"eur",  "towgs84=-84.0,-103.0,-127.0",  "international",  "European",
"tokyo",  "towgs84=-128.0,481.0,664.0",  "bessel",  "Tokyo mean",
"aus",  "towgs84=-122.0,-41.0,146.0",  "australian",  "Australian Geodetic",
"osgb36",  "towgs84=368.0,-120.0,425.0",  "airy",  "Ordnance Survey of Great Britain",
"sam69",  "towgs84=-77.0,3.0,-45.0",  "sam69",  "South American 1969",
"SAD-69",  "towgs84=-60.0,-2.0,-41.0",  "SAD-69",  "SAD-69/Brasil",
"pulkovo",  "towgs84=28.0,-130.0,-95.0",  "krassovsky",  "Pulkovo 1942",
"eur50",  "towgs84=-87.0,-98,-121",  "international",  "European 1950 mean",
"eur79",  "towgs84=-86,-98,-119",  "international",  "European 1979 mean",
"agd66",  "towgs84=-133,-48,148",  "australian",  "Australian Geodetic 1966",
"agd84",  "towgs84=-134,-48,149",  "australian",  "Australian Geodetic 1984",
"grs80",  "towgs84=0.0,0.0,0.0",  "wgs84",  "Geographic Reference System 1980",
"rome40",  "towgs84=-225,-65,9",  "international",  "Rome 1940",
"Sasia",  "towgs84=7.0,-10.0,-26.0",  "fschr60m",  "South Asia",
"S-42",  "towgs84=28.0,-121.0,-77.0",  "krassovsky",  "S-42",
"potsdam",  "towgs84=606.0,23.0,413.0",  "bessel",  "Potsdam Rauenberg 1950 DHDN",
"carthage",  "towgs84=-263.0,6.0,431.0",  "clark80",  "Carthage 1934 Tunisia",
NULL,       NULL,                                    NULL,    NULL ,
};
