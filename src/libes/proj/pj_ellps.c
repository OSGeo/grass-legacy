/* definition of standard geoids */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_ellps.c,v 4.1 1992/04/19 16:10:56 gie Exp $";
#endif
#define __PJ_ELLPS
#include "projects.h"
	struct PJ_ELLPS
pj_ellps[] = {
"MERIT",	"a=6378137.0", "rf=298.257", "MERIT 1983",
"GRS80",	"a=6378137.0", "rf=298.257222", "GRS 1980(IUGG, 1980)",
"IAU76",	"a=6378140.0", "rf=298.257", "IAU 1976",
"airy",		"a=6377563.396", "b=6356256.91", "Airy 1830",
"aust_ntl",	"a=6378160.0", "rf=298.25", "Australian Natl, S. Amer., IAU 64",
"GRS67",	"a=6378160.0", "rf=247.247167", "GRS 67(IUGG 1967)",
"bessel",	"a=6377397.155", "rf=299.152813", "Bessel 1841",
"clrk66",	"a=6378206.4", "b=6356583.8", "Clarke 1866",
"clrk80",	"a=6378249.145", "rf=293.4663", "Clarke 1880 mod.",
"everest",	"a=6377276.3452", "b=6356075.4133", "Everest 1830",
"hough",	"a=6378270.0", "b=6356794.343479", "Hough",
"intl",		"a=6378388.0", "rf=297.", "International 1909 (Hayford)",
"krass",	"a=6378245.0", "rf=298.3", "Krassovsky, 1942",
"mercury",	"a=6378166.0", "b=6356784.283666", "Mercury 1960",
"mod_airy",	"a=6377341.89", "b=6356036.143", "Modified Airy",
"mod_ever",	"a=6377304.063", "b=6356103.039", "Modified Everest",
"mod_merc",	"a=6378150.0", "b=6356768.337303", "Modified Merc 1968",
"new_intl",	"a=6378157.5", "b=6356772.2", "New International 1967",
"SEasia",	"a=6378155.0", "b=6356773.3205", "Southeast Asia",
"walbeck",	"a=6376896.0", "b=6355834.8467", "Walbeck",
"WGS66",	"a=6378145.0", "b=6356759.769356", "WGS 66",
"WGS72",	"a=6378135.0", "b=6356750.519915", "WGS 72",
"sphere",	"a=6370997.0", "es=0.0", "Sphere of 6370997 m",
(char *)0, (char *)0, (char *)0, (char *)0
};
