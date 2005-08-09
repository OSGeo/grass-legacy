#include "pngdriver.h"

int linewidth = 1;

/* changes the width of line */
int
line_width(int width)
{
	linewidth = width;

	return 0;
}
