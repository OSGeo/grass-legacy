/* int angle_ok (double test, double angle, double tolerance) */
int 
angle_ok (double test, double angle, double tolerance)
/*-
 * Tests whether direction {\tt test} is approximately the same direction
 * as {\tt angle} ($\pm\/$ {\tt tolerance}). Returns 1 if so, 0 otherwise.
 */
{
  double upper, lower;

  upper = angle + tolerance;
  lower = angle - tolerance;

  /* ( 0 > i >= 360) for all i in {upper, lower, test} */
  while (upper > 360.0)
    upper -= 360.0;
  while (upper <= 0.0)
    upper += 360.0;
  while (lower > 360.0)
    lower -= 360.0;
  while (lower <= 0.0)
    lower += 360.0;
  while (test > 360.0)
    test -= 360.0;
  while (test <= 0.0)
    test += 360.0;

  if (lower > upper)
    upper += 360;
  if (test < lower)
    test += 360;

#ifdef TESTING
  fprintf (stdout,"%g %g %g\n", lower, test, upper);
#endif
  /* now check to see if the angle is ok */
  if ((test >= lower) && (test <= upper))
    return 1;
  else
    return 0;
}

#ifdef TESTING
#include<stdio.h>
int 
main (void)
{
  fprintf (stdout,"yes %d\n", angle_ok (356.0, 360.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360 + 356.0, 360.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (356.0, 360 + 360.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (356.0, 360.0, 360 + 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (4.0, 360.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360 + 4.0, 360.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (4.0, 360 + 360.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (4.0, 360.0, 360 + 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360.0, 4.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360 + 360.0, 4.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360.0, 360 + 4.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360.0, 4.0, 360 + 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (0.0, 4.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360 + 0.0, 4.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (0.0, 360 + 4.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (0.0, 4.0, 360 + 5.0));
  fprintf (stdout,"no %d\n", angle_ok (0.0, 14.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (360 + 0.0, 14.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (0.0, 360 + 14.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (0.0, 14.0, 360 + 5.0));
  fprintf (stdout,"no %d\n", angle_ok (1000.0, 14.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (360 + 1000.0, 14.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (1000.0, 360 + 14.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (1000.0, 14.0, 360 + 5.0));
  fprintf (stdout,"no %d\n", angle_ok (10.0, 720.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (360 + 10.0, 720.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (10.0, 360 + 720.0, 5.0));
  fprintf (stdout,"no %d\n", angle_ok (10.0, 720.0, 360 + 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (4.0, 720.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (360 + 4.0, 720.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (4.0, 360 + 720.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (4.0, 720.0, 360 + 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (-4.0, 720.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (-4.0 + 360, 720.0, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (-4.0, 720.0 + 360, 5.0));
  fprintf (stdout,"yes %d\n", angle_ok (-4.0, 720.0, 5.0 + 360));
}
#endif				/* TESTING */
