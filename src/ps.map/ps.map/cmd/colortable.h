/* Header file: colortable.h
**
** Author: Paul W. Carlson	April 1992
*/

#include <stdio.h>

struct colortable {
	double x, y, width;
	double height; /* fp legend height */
	char *font;
	int fontsize;
	int color;
	int cols;
	int nodata;
};

#ifdef MAIN
struct colortable ct;
#else
extern struct colortable ct;
#endif
