#include "pngdriver.h"

void PNG_draw_point(int x, int y)
{
	int c, r, g, b, r2, g2, b2, ind;

	if (x < 0 || x >= width || y < 0 || y >= height)
		return;

	ind = y * width + x;
	c = currentColor;
	if (transparency > 0.0) {
		DRV_lookup_rgb(grid[ind], &r, &g, &b);
		DRV_lookup_rgb(currentColor, &r2, &g2, &b2);
		c = DRV_lookup_color(
				r*transparency+r2*(1-transparency),
				g*transparency+g2*(1-transparency),
				b*transparency+b2*(1-transparency));
	}
	grid[ind] = c;
}

