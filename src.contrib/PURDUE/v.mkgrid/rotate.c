#include<math.h>
/* rotates x and y about the origin (xo,yo) by angle radians */
void rotate (x, y, xo, yo, angle)
  double *x, *y, xo, yo, angle;
{
  double tmpx, tmpy;

  /* first translate */
  tmpx = *x - xo;
  tmpy = *y - yo;

  /* now rotate */
  *x = tmpx * cos (angle) - tmpy * sin (angle);
  *y = tmpx * sin (angle) + tmpy * cos (angle);

  /* now translate back */
  *x += xo;
  *y += yo;
}
