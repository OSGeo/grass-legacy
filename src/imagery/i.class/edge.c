int edge_point(register int, register int);

int edge (register int x0, register int y0, int x1, int y1)
{
	register float m;
	register float x;


	if (y0 == y1) return 0;

	x = x0;
	m = (float) (x0 - x1) / (float) (y0 - y1) ;

	if (y0 < y1)
		while (++y0 < y1)
		{
			x0 = (x += m) + .5;
			edge_point (x0, y0);
		}
	else
		while (--y0 > y1)
		{
			x0 = (x -= m) + .5;
			edge_point (x0, y0);
		}

	return 0;
}
