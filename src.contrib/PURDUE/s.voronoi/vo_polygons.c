/*- void write_polygons (Map, tmpfile, verbose) // no return value
 * struct Map_info *Map;                 // map struct, including file pointer
 * char *tmpfile;                        // output of sweep2
 * int verbose;                          // talk flag
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "display.h"
#include "Vect.h"
#include "vo_defs.h"

extern struct Cell_head window;

void 
write_polygons (struct Map_info *Map, char *tmpfile, int verbose)
{
  char *r, buf[128];
  int draw, b1, b2;
  int v_alloc, e_alloc, s_alloc;	/* amount allocated */
  int e_max;			/* max number read */
  int i, si, ci, vi, ei;	/* counters */
  int **e;			/* array of vertex indices */
  double **v;			/* array of vertices */
  double **s;			/* array of sites */
  double tmpx[2], tmpy[2];
  float a, b, c;
  struct line_pnts *Points;
  FILE *ptr;

  if ((ptr = fopen (tmpfile, "r")) == NULL)
    G_fatal_error ("Cannot open temporary file");
  if (verbose)
    fprintf (stderr, "Finding Theissen polygons ...       ");
  Points = Vect_new_line_struct ();
  e_alloc = 1000;
  e = (int **) G_malloc (e_alloc * sizeof (int *));
  for (ei = 0; ei < e_alloc; ++ei)
  {
    e[ei] = (int *) G_malloc (3 * sizeof (int));
    if (e[ei] == NULL)
      G_fatal_error ("Insufficent memory for endpoints");
  }
  v_alloc = 1000;
  v = (double **) G_malloc (v_alloc * sizeof (double *));
  for (vi = 0; vi < v_alloc; ++vi)
  {
    v[vi] = (double *) G_malloc (3 * sizeof (double));
    if (v[vi] == NULL)
      G_fatal_error ("Insufficent memory for vertices");
  }
  s_alloc = 1000;
  s = (double **) G_malloc (s_alloc * sizeof (double *));
  for (si = 0; si < s_alloc; ++si)
  {
    s[si] = (double *) G_malloc (2 * sizeof (double));
    if (s[si] == NULL)
      G_fatal_error ("Insufficent memory for vertices");
  }
  ei = 0;
  while (fgets (buf, 128, ptr) != NULL)
  {
    switch (buf[0])
    {
    case 'e':			
      if (ei > e_alloc)
	G_fatal_error ("Need to realloc for endpoints");
      sscanf (buf, "e %d %d %d", &e[ei][2], &e[ei][0], &e[ei][1]);
      ei++;
      break;
    case 's':			
      sscanf (buf, "site (%d) at %*g %*g", &si);
      if (si > s_alloc)
	G_fatal_error ("Need to realloc for sites");
      sscanf (buf, "site (%*d) at %g %g", &a, &b);
      s[si][0]=(double) a;
      s[si][1]=(double) b;
      break;
    case 'v':			
      sscanf (buf, "vertex(%d) at %*g %*g", &vi);
      if (vi > v_alloc)
	G_fatal_error ("Need to realloc for vertices");
      sscanf (buf, "vertex(%*d) %*s %f %f", &a, &b);
      v[vi][0] = (double) a;
      v[vi][1] = (double) b;
      break;
    default:
      break;
    }
  }
  e_max = ei - 1;
  e_max = ei ;
  for (ei = 0; ei < e_max; ++ei)
  {
    if (verbose)
      G_percent (ei, e_max, 1);
    if (e[ei][0] == -1)
    {
      vi = 1;
      ci = 0;
      draw = 0;
    }
    else if (e[ei][1] == -1)
    {
      vi = 0;
      ci = 1;
      draw = 0;
    }
    else
    {
      tmpx[0] = v[e[ei][0]][0];
      tmpy[0] = v[e[ei][0]][1];
      tmpx[1] = v[e[ei][1]][0];
      tmpy[1] = v[e[ei][1]][1];
      draw = 1;
    }

    if (draw == 0)
    {
      if( in_region (v[e[ei][vi]][0], v[e[ei][vi]][1])) 
      {
        tmpx[vi] = v[e[ei][vi]][0];
        tmpy[vi] = v[e[ei][vi]][1];
        rewind (ptr);		/* go back and read lines to get equation */

/* line(3) 0.0285714x+1y=32.7714, bisecting 1 0 */

        i=-9;
        while ((r = fgets (buf, 128, ptr)) != NULL && i != e[ei][2])
	  sscanf (buf, "line(%d) %fx+%fy=%f, bisecting %d %d", 
                  &i, &a, &b, &c, &b1, &b2);
        if (r == NULL)
	  G_fatal_error ("Could not find line");
        if (extend_line (window.south, window.north, window.west, window.east,
	    a, b, c, v[e[ei][vi]][0], v[e[ei][vi]][1], &tmpx[ci], &tmpy[ci],
            s[b1][0], s[b1][1], s[b2][0], s[b2][1]))
        {
	  draw = 1;
        }
        else
        {
	  draw = 0;
        }
      }
    }

    if ( !in_region(tmpx[0],tmpy[0]) && !in_region(tmpx[1],tmpy[1]) )
      draw=0;

    if (draw)
    {
      D_clip( window.south, window.north, window.west, window.east,
              &tmpx[0], &tmpy[0], &tmpx[1], &tmpy[1]);
      if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
	G_fatal_error ("Out of memory");
      Vect_write_line (Map, AREA, Points);
    }
  }

  /* add border */
   tmpx[0]=window.east; tmpy[0]=window.south;
   tmpx[1]=window.east; tmpy[1]=window.north;
   if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
     G_fatal_error ("Out of memory");
   Vect_write_line (Map, AREA, Points);

   tmpx[0]=window.east; tmpy[0]=window.north;
   tmpx[1]=window.west; tmpy[1]=window.north;
   if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
     G_fatal_error ("Out of memory");
   Vect_write_line (Map, AREA, Points);

   tmpx[0]=window.west; tmpy[0]=window.north;
   tmpx[1]=window.west; tmpy[1]=window.south;
   if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
     G_fatal_error ("Out of memory");
   Vect_write_line (Map, AREA, Points);

   tmpx[0]=window.west; tmpy[0]=window.south;
   tmpx[1]=window.east; tmpy[1]=window.south;
   if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
     G_fatal_error ("Out of memory");
   Vect_write_line (Map, AREA, Points);

  for (ei=0;ei<e_alloc;ei++) G_free(e[ei]);
  G_free (e);
  for (vi=0;vi<v_alloc;vi++) G_free(e[vi]);
  G_free (v);
  for (si=0;si<s_alloc;si++) G_free(e[si]);
  G_free (s);

  Vect_destroy_line_struct (Points);
  fclose (ptr);
  if (verbose)
    G_percent (1, 1, 1);
}
