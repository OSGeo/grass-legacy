#include "pngdriver.h"

int linewidth = 0;

int PNG_Line_width(int width)
{
	int ret = linewidth;

	if(width >= 0)
		linewidth = (width < 0 ? 0 : width);

	return ret;
}

