#include<stdio.h>
#include<math.h>
main ()
{
  int i, j, x, y, cat;
  char line[80], desc[60];
  double xo, yo, angle;

  /* set these three things */
  xo = -1;
  yo = -1;
  angle = 45 * 3.1415927 / 180;	/* 20 degrees in radians */

/*
  fscanf (stdin, "%s\n", line);
  fprintf (stdout, "%s\n", line);

  fscanf (stdin, "%s\n", line);
  fprintf (stdout, "%s\n", line);
*/
  while ( fscanf (stdin, "%d|%d|#%d %s", &x, &y, &cat, desc) != EOF) 
  {
    fprintf (stdout, "%d|%d|#%d %s\n", x, y, cat, desc);
/*-
    fprintf (stdout, "%f|%f|#%d %s\n",
	      ((x - xo) * cos (angle) - (y - yo) * sin (angle)),
	      ((x - xo) * sin (angle) + (y - yo) * cos (angle)),
	     cat, desc);
*/
  }
}
