
#include <stdlib.h>

#include "gis.h"
#include "pngdriver.h"

struct point
{
	int x, y;
};

static int
cmp_int(const void *aa, const void *bb)
{
	const int *a = aa;
	const int *b = bb;

	return *a - *b;
}

static void
fill(int x0, int x1, int y)
{
	unsigned int *p;
	int x;

	if (x1 < 0)
		return;

	if (x0 >= width)
		return;

	if (x0 < 0)
		x0 = 0;

	if (x1 > width)
		x1 = width;

	p = &grid[y * width + x0];

	for (x = x0; x < x1; x++)
		*p++ = currentColor;
}

static void
line(const struct point *p, int n, int y)
{
	static int *xs;
	static int max_x;
	int num_x = 0;
	int i;

	for (i = 0; i < n; i++)
	{
		const struct point *p0 = &p[i];
		const struct point *p1 = &p[i + 1];
		const struct point *tmp;
		long x;

		if (p0->y == p1->y)
			continue;

		if (p0->y > p1->y)
			tmp = p0, p0 = p1, p1 = tmp;

		if (p0->y > y)
			continue;

		if (p1->y <= y)
			continue;

		x = p1->x * (y - p0->y) + p0->x * (p1->y - y);
		x /= p1->y - p0->y;

		if (num_x >= max_x)
		{
			max_x += 20;
			xs = G_realloc(xs, max_x * sizeof(int));
		}

		xs[num_x++] = x;
	}

	qsort(xs, num_x, sizeof(int), cmp_int);

	for (i = 0; i + 1 < num_x; i += 2)
		fill(xs[i], xs[i + 1], y);
}

static void
poly(const struct point *p, int n)
{
	int y0, y1;
	int i, y;

	if (n < 3)
		return;

	y0 = y1 = p[0].y;

	for (i = 1; i < n; i++)
	{
		if (y0 > p[i].y)
			y0 = p[i].y;

		if (y1 < p[i].y)
			y1 = p[i].y;
	}

	if (y1 < 0 || y0 > height)
		return;

	if (y0 < 0)
		y0 = 0;

	if (y1 > height)
		y1 = height;

	for (y = y0; y < y1; y++)
		line(p, n, y);
}

int
Polygon_abs(int *xarray, int *yarray, int count)
{
	static struct point *points;
	static int max_points;
	int i;

	if (max_points < count + 1)
	{
		max_points = count + 1;
		points = G_realloc(
			points, sizeof(struct point) * max_points);
	}

	for (i = 0; i < count; i++)
	{
		points[i].x = xarray[i];
		points[i].y = yarray[i];
	}

	points[count].x = xarray[0];
	points[count].y = yarray[0];

	poly(points, count);

	modified = 1;

	return 0;
}

