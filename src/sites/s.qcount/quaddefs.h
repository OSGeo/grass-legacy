/*
 * Copyright (C) 1994-1995. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
int *count_sites (SITE_XYZ *, int, int *, double, SITE_XYZ *, int);
SITE_XYZ *find_quadrats (int, double, struct Cell_head, int);
void qindices (int *, int, double *, double *, 
		double *, double *, double *, double *);
