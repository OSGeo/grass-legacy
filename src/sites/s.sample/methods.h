/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#define NMETHODS 3
#define NEAREST 1
#define BILINEAR 2
#define CUBIC 3

#ifdef USE_OLD_CODE
struct zstruct
{
  double x, y; 
  char desc[80];
};
typedef struct zstruct Z;

int readsites();
#endif
double nearest ();
double bilinear ();
double scancatlabel ();
double cubic ();
FILE * opensites();
