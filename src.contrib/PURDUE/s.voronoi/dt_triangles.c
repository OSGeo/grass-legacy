/*- void write_triangles (Map, tmp_sites, tmp_vert, verbose) // no return val
 * struct Map_info *Map;                 // map struct, including file pointer
 * char *tmp_sites;                      // output of sweep2 -d
 * char *tmp_vert;                       // output of sweep2 -t
 * int verbose;                          // talk flag
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

extern struct Cell_head window;

void 
write_triangles (struct Map_info *Map, char *tmp_sites, char *tmp_vert, int verbose)
{
  char buf[128];
  int v_alloc, s_alloc;		/* amount allocated */
  int s_max;			/* max number read */
  int vi, si;		        /* counters */
  int **s;			/* array of vertex indices */
  double **v;			/* array of vertices */
  double tmpx[3], tmpy[3];
  float a, b;
  struct line_pnts *Points;
  FILE *ptr;

  if (verbose)
    fprintf (stderr, "Doing Delaunay triangulation ...    ");
  Points = Vect_new_line_struct ();
  v_alloc = 1000;
  v = (double **) G_malloc (v_alloc * sizeof (double *));
  for (vi = 0; vi < v_alloc; ++vi)
  {
    v[vi] = (double *) G_malloc (2 * sizeof (double));
    if (v[vi] == NULL)
      G_fatal_error ("Insufficent memory for vertices");
  }
  if ((ptr = fopen (tmp_sites, "r")) == NULL)
    G_fatal_error ("Cannot open temporary file");
  while (fgets (buf, 128, ptr) != NULL)
  {
    switch (buf[0])
    {
    /* site (27) at 0.841259 0.018226 */
    case 's':			
      sscanf (buf, "site (%d) at %*g %*g", &vi);
      if (vi > v_alloc)
	G_fatal_error ("Need to realloc for vertices");
      sscanf (buf, "site (%*d) %*s %f %f", &a, &b);
      v[vi][0] = (double) a;
      v[vi][1] = (double) b;
      break;
    default:
      break;
    }
  }
  fclose(ptr);

  s_alloc = 1000;
  s = (int **) G_malloc (s_alloc * sizeof (int *));
  for (si = 0; si < s_alloc; ++si)
  {
    s[si] = (int *) G_malloc (3 * sizeof (int));
    if (s[si] == NULL)
      G_fatal_error ("Insufficent memory for sites indices");
  }
 
  if ((ptr = fopen (tmp_vert, "r")) == NULL)
    G_fatal_error ("Cannot open temporary file");
  si=0;
  while (fgets (buf, 128, ptr) != NULL)
  {
    sscanf (buf, "%d %d %d", &s[si][0], &s[si][1], &s[si][2] );
    si++;
  }

  fclose(ptr);

  s_max = si ;
  for (si = 0; si < s_max; ++si)
  {
    if (verbose)
      G_percent (si, s_max, 1);

    tmpx[0] = v[s[si][0]][0]; tmpy[0] = v[s[si][0]][1];
    tmpx[1] = v[s[si][1]][0]; tmpy[1] = v[s[si][1]][1];
    if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
	G_fatal_error ("Out of memory");
    Vect_write_line (Map, AREA, Points);

    tmpx[0] = v[s[si][1]][0]; tmpy[0] = v[s[si][1]][1];
    tmpx[1] = v[s[si][2]][0]; tmpy[1] = v[s[si][2]][1];
    if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
	G_fatal_error ("Out of memory");
    Vect_write_line (Map, AREA, Points);

    tmpx[0] = v[s[si][0]][0]; tmpy[0] = v[s[si][0]][1];
    tmpx[1] = v[s[si][2]][0]; tmpy[1] = v[s[si][2]][1];
    if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
	G_fatal_error ("Out of memory");
    Vect_write_line (Map, AREA, Points);
  }

  for (si = 0; si < s_alloc; ++si) G_free(s[si]);
  G_free (s);
  for (vi = 0; vi < v_alloc; ++vi) G_free(v[vi]);
  G_free (v);
  Vect_destroy_line_struct (Points);
  if (verbose)
    G_percent (1, 1, 1);
}
