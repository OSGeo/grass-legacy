/* compute square of distance of point (x,y) to line segment (x1,y1 - x2,y2) */

#define ZERO(x) ((x) < tolerance && (x) > -tolerance)
#define TOLERANCE 1.0e-10
static double tolerance = TOLERANCE;

int 
dig_set_distance_to_line_tolerance (double t)
{
  if (t <= 0.0)
    t = TOLERANCE;
  tolerance = t;

  return 0;
}

/* works correctly for  x1==x2 && y1==y2 */
double 
dig_distance2_point_to_line (
			      double x, double y,	/* point */
			      double x1, double y1, double x2, double y2)	/* line segment */
{
  register double dx, dy;
  double t;

  dx = x2 - x1;
  dy = y2 - y1;

  if (ZERO (dx) && ZERO (dy))	/* line is degenerate */
    {
      dx = x1 - x;
      dy = y1 - y;
      return dx * dx + dy * dy;	/* compute distance x,y to x1,y1 */
    }

  t = (dx * (x - x1) + dy * (y - y1)) / (dx * dx + dy * dy);

  if (t < 0.0)			/* go to x1,y1 */
    t = 0.0;
  else if (t > 1.0)		/* go to x2,y2 */
    t = 1.0;

/* go t from x1,y1 towards x2,y2 */
  dx = dx * t + x1 - x;
  dy = dy * t + y1 - y;

  return dx * dx + dy * dy;
}

/*
   ** dig_xy_distance2_point_to_line ()
   ** same as dig_distance2_point_to_line () except px,py are pointers to doubles
   ** and the line intersection is returned in them
 */

double 
dig_xy_distance2_point_to_line (
				 double *px, double *py,	/* point */
				 double x1, double y1, double x2, double y2)	/* line segment */
{
  register double dx, dy;
  register double x, y;
  double t;

  x = *px;
  y = *py;
  dx = x2 - x1;
  dy = y2 - y1;

  if (ZERO (dx) && ZERO (dy))	/* line is degenerate */
    {
      dx = x1 - x;
      dy = y1 - y;
      goto end;
    }

  t = (dx * (x - x1) + dy * (y - y1)) / (dx * dx + dy * dy);

  if (t < 0.0)			/* go to x1,y1 */
    t = 0.0;
  else if (t > 1.0)		/* go to x2,y2 */
    t = 1.0;

/* go t from x1,y1 towards x2,y2 */
  dx = dx * t + x1 - x;
  dy = dy * t + y1 - y;

end:
  *px = x + dx;
  *py = y + dy;
  return dx * dx + dy * dy;
}

/*
   ** dig_xy_distance3_point_to_line ()
   ** same as dig_distance2_point_to_line () except returns value in status
   **   telling if point is w/in segment space, or past ends
   **   
   **  status = 0 if ok, -1 if t < 0  and 1 if t > 1
   ** 
 */

double 
dig_xy_distance3_point_to_line (
				 double *px, double *py,	/* point */
				 double x1, double y1, double x2, double y2,	/* line segment */
				 int *status)
{
  register double dx, dy;
  register double x, y;
  double t;

  *status = 0;

  x = *px;
  y = *py;
  dx = x2 - x1;
  dy = y2 - y1;

  if (ZERO (dx) && ZERO (dy))	/* line is degenerate */
    {
      dx = x1 - x;
      dy = y1 - y;
      goto end;
    }

  t = (dx * (x - x1) + dy * (y - y1)) / (dx * dx + dy * dy);

  if (t < 0.0)			/* go to x1,y1 */
    t = 0.0, *status = -1;
  else if (t > 1.0)		/* go to x2,y2 */
    t = 1.0, *status = 1;

/* go t from x1,y1 towards x2,y2 */
  dx = dx * t + x1 - x;
  dy = dy * t + y1 - y;

end:
  *px = x + dx;
  *py = y + dy;
  return dx * dx + dy * dy;
}
