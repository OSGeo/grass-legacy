graph_line (x0,y0,x1,y1)

	register int x0, y0 ;
	int x1, y1 ;
{
	int dx, dy;
	int xinc, yinc;

	register res1;
	int res2;

	xinc = 1;
	yinc = 1;
	if ((dx = x1-x0) < 0) 
	{
		xinc = -1;
		dx = -dx;
	}
	if ((dy = y1-y0) < 0) 
	{
		yinc = -1;
		dy = -dy;
	}
	res1 = 0;
	res2 = 0;

	if (dx > dy)
		while (x0 != x1)
		{
			graph_point (x0, y0);
			if (res1 > res2)
			{
				res2 += dx - res1;
				res1 = 0;
				y0 += yinc;
			}
			res1 += dy;
			x0 += xinc;
		}
	else if (dx < dy)
		while (y0 != y1)
		{
			graph_point (x0, y0);
			if (res1 > res2)
			{
				res2 += dy - res1;
				res1 = 0;
				x0 += xinc;
			}
			res1 += dx;
			y0 += yinc;
		}
	else
		while (x0 != x1)
		{
			graph_point (x0, y0);
			y0 += yinc;
			x0 += xinc;
		}

	graph_point (x1, y1);
}
