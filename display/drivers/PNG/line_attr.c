#include "pngdriver.h"

int linewidth = 0;

/* changes the width of line */
int
line_width(int width)
{
	linewidth = (width < 0 ? 0 : width);

	return 0;
}
