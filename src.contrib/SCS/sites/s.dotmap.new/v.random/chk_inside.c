double
INCHECK(map, X, Y, Area)
    struct Map_info *map;
    double X, Y;
    P_AREA *Area;
{
    static int first_time;
    static struct line_pnts Points;
    double *x, *y;
    double cur_min;
    double cur_x, cur_y;
    double dig_x_intersect();
    double x_inter;
    double fabs();
    double x_dist;
    int at_line;
    int n_intersects;
    int n_segs;
    int l;
    int n;
    P_LINE *Line;

    cur_min = HUGE_VAL;
    cur_x = 0.0;
    cur_y = 0.0;
    n_intersects = 0;

    if (first_time == 0)	/* executes once */
    {
	Points.alloc_points = Points.n_points = 0;
	first_time = -1;
    }

/* Get-line loop */
    for(l=0; l<Area->n_lines; l++)
    {
    /* Read in line coordinates */
	at_line = ABS(Area->lines[l]);

	Line = &(map->Line[at_line]);

	/* dont check lines that obviously do not 
	** intersect with test ray    -dpg 3.1
	*/
	if ((Line->N < Y) || (Line->S > Y) || (Line->E < X))
	    continue;

	if (0 > V2_read_line (map, &Points, at_line))
	    return (-1.0);
    
    /* adjust yarray coordinates */
	y = Points.y;
	for(n=0; n<Points.n_points; n++)
	{
/*  THIS WONT WORK IF Y == 0.0   12/89 */
/* THIS is changing USERS data!! */
	    if (*y == Y)
		*y = Y * 1.000000001;
		/* *y = Y + .001;  changed to depend more on scale -dpg 6/89 */
	    y++;
	}

    /* Point loop */
	x = Points.x;
	y = Points.y;
	cur_x = *x ; x++;
	cur_y = *y ; y++;
	n_segs = Points.n_points - 1;
    
	for(n=0; n<n_segs; n++)
	{
	    if((cur_y < Y && *y < Y)
	    || (cur_y > Y && *y > Y)
	    || (cur_x < X && *x < X))
	    {
		cur_x = *x ; x++;
		cur_y = *y ; y++;
		continue;
	    }

	    x_inter = dig_x_intersect (cur_x, *x, cur_y, *y, Y);
	/*DEBUG*/fprintf(stderr,"INTER %lf %lf %lf %lf %lf %lf\n",x_inter,cur_x, *x, cur_y, *y, Y);
	    if (x_inter > X)
	    {
		n_intersects++;

		x_dist = fabs(x_inter - X);
		if (x_dist < cur_min)
		    cur_min = x_dist;
	    }

	    cur_x = *x ; x++;
	    cur_y = *y ; y++;
	}
    }
/*DEBUG*/ fprintf (stderr,"PNT_IN_AREA:  intersects = %d\n", n_intersects);
    if (n_intersects % 2)
	return(cur_min);
    else
	return(0.0);
}

double
dig_x_intersect (beg_x, end_x, beg_y, end_y, Y) 
    double beg_x;
    double end_x;
    double beg_y;
    double end_y;
    double Y;
{
    double b, a;

    b = (end_x - beg_x) / (end_y - beg_y);
    a = beg_x - b * beg_y;
    return(a + b * Y);
}

