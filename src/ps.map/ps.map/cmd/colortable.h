/* Header file: colortable.h
**
** Author: Paul W. Carlson	April 1992
*/

#include <stdio.h>

struct colortable {
	double x, y, width;
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
