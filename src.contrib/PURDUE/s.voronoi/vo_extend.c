#include <stdio.h>
#include <math.h>

/*-extend_line()  finds coordinates along the boundary of a window
 *                  that also lie on a specified line (ax+by=c). The
 *                  line can cross at least two boundaries---the line
 *                  that intersects the midpoint of s1 and s2 determines 
 *                  which coordinates are placed in (c_x, c_y).
 *
 * The limits of the window are described by:
 *    e:  east
 *    w:  west
 *    s:  south
 *    n:  north
 * Note that the following constraints must be true:
 *    ( w < e )     ( s < n )
 *
 *    x and y are points on the line ax + by = c that are assumed
 *    to lie within the window.
 *
 *    the c_x and c_y values are changed.
 *
 * returns: 0 on error, 1 otherwise
 */

/*
	ael:
	1) I see the potential below for some divisions by zero: need to take care of those.
 */

extend_line (s, n, w, e,
	     a, b, c,
	     /* the xy are the vertex under consideration */
	     x, y,
	     /* the c's will be returned as the point to map to */
	     c_x, c_y,
	     /* the s's are the two points separated by this line */
	     s1_x, s1_y, s2_x, s2_y)
     double s, n, w, e, a, b, c, x, y, *c_x, *c_y, s1_x, s1_y, s2_x, s2_y;
{
  int i, in_region () ;
  double nx[4], ny[4], mx,my; 
  
  if (x > w && x < e && y > s && y < n)
    {
      nx[0] = e;
      ny[0] = (c - a * nx[0]) / b;
      
      nx[1] = w;
      ny[1] = (c - a * nx[1]) / b;
      
      ny[2] = s;
      nx[2] = (c - b * ny[2]) / a;
      
      ny[3] = n;
      nx[3] = (c - b * ny[3]) / a;
      
      /* midpoint of the two associated sites.
      mx = s1_x + (s2_x - s1_x)/2.0;
      my = s1_y + (s2_y - s1_y)/2.0;
      which is the wrong quantity to focus on. */
      
      /* use this as a temp for the slope: */
      my = (s2_y - s1_y)/(s2_x - s1_x);

      mx = (c+b*(my*s2_x - s2_y))/(a + b*my);
      my = s2_y+my*(mx-s2_x);
      /* ael */
       
      for(i=0; i<4; ++i)
	{
	  /* if we're in the region */
	  if (in_region(nx[i],ny[i])
	      &&
	      /* each of the two constraints following gets some of them right
		 and some of them wrong. I need to figure out under what conditions this
		 happens to get the solution....
		 
		 Here's the problem: the midpoint of the two sites separated by the line
		 can fall either on the "left" or "right": we need to know also the angle
		 of the two lines which intersect at that vertex (i.e., their a and b).
		 With that, we can decide definitively...

		 For the moment, I'm just going to make this false, and forget about it...!
		 */
	      ((nx[i] <= x && x <= mx) || (nx[i] >= x && x >= mx))
	      &&
	      ((ny[i] <= y && y <= my) || (ny[i] >= y && y >= my))
	      /*
	      ((nx[i] <= x && x <= mx) || (nx[i] >= x && x >= mx))
	      &&
	      ((ny[i] <= y && y <= my) || (ny[i] >= y && y >= my))
	      0
	      ((nx[i] <= mx && mx <= x) || (nx[i] >= mx && mx >= x) )
	      &&
	      ((ny[i] <= my && my <= y) || (ny[i] >= my && my >= y) )
	      */
	      ) 
	    { 
	      /* I added these to try to understand the conditions under
		 which things go awry */
	      /*
		fprintf(stderr,"\nedges: \n((%g %g) (%g %g) (%g %g) (%g %g)\n",nx[0],ny[0],nx[1],ny[1],nx[2],ny[2],nx[3],ny[3]);
		fprintf(stderr,"\n(%g %g) (%g %g) (%g %g))\n",x,y,mx,my,nx[i],ny[i]);
		*/
	      fprintf(stderr,"\nDIAG: x=%g y=%g mx=%g my=%g nx=%g ny=%g\n",x,y,mx,my,nx[i],ny[i]);

	      *c_x = nx[i];
	      *c_y = ny[i];
	      return 1;
	    }
	}
    }
  fprintf(stderr,"\nDIAG: extend line failed\n");
  return 0;
}


/*
	      ((nx[i] <= x && x <= mx) || (nx[i] >= x && x >= mx))
	      &&
	      ((ny[i] <= y && y <= my) || (ny[i] >= y && y >= my))
	      */

	  /*         ( (nx[i] <= x && mx <= x) || (nx[i] >= x && mx >= x) )  &&
		     ( (ny[i] <= y && my <= y) || (ny[i] >= y && my >= y) ) )  */

	    /* ( (nx[i] <= x && x <= mx) || (nx[i] >= x && x >= mx) )  &&
	       ( (ny[i] <= y && y <= my) || (ny[i] >= y && y >= my) ) )  */
