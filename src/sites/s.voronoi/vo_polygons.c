/*- void write_polygons (Map, tmpfile, verbose) // no return value
 * struct Map_info *Map;                 // map struct, including file pointer
 * char *tmpfile;                        // output of sweep2
 * int verbose;                          // talk flag
 */

#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "vo_defs.h"

#define ALLOC_VOR 1000
struct VorLine {
    double a, b, c;
};

struct VorVertex {
    double x, y;
};


extern struct Cell_head window;

void
write_polygons (struct Map_info *Map, char *tmpfile, int verbose)
{
  char *r, buf[128];
  int v_alloc, l_alloc; /* amount allocated */
  int i, vi, li;	    /* counters */
  int vLeft, vRight, lineIndex, vIndex; /* array indices */
  int knownPointAtLeft;                 /* boolean, placement of the only known endpoint
                                           in a semirect (left or right) */
  struct VorVertex *v;                  /* array of vertices */
  struct VorLine *l;                    /* array of line definitions */
  double a, b, c;                        /* line parameters */

  double tmpx[2], tmpy[2];              /* data structures for output in GRASS format */
  struct line_pnts *Points;
  FILE *ptr;

  if ((ptr = fopen (tmpfile, "r")) == NULL)
    G_fatal_error ("Cannot open temporary file");
  if (verbose)
    fprintf (stderr, "Processing Voronoi diagram\n");

  prepareBoundaryArrays(window);
  Points = Vect_new_line_struct ();
  l_alloc = ALLOC_VOR;
  l = (struct VorLine *) G_malloc(l_alloc * sizeof(struct VorLine));
  v_alloc = ALLOC_VOR;
  v = (struct VorVertex *) G_malloc(v_alloc * sizeof(struct VorVertex));

  while (fgets (buf, 128, ptr) != NULL)
  {
    switch (buf[0])
    {
    case 'l':
        sscanf (buf, "line(%d) %lfx+%lfy=%lf, bisecting %*d %*d", &li, &a, &b, &c);
        if(li >= l_alloc) {
            l_alloc += ALLOC_VOR;
            l = (struct VorLine*) G_realloc(l, l_alloc * sizeof(struct VorLine));
        }
        l[li].a = a;
        l[li].b = b;
        l[li].c = c;
        break;
    case 'v':
        sscanf (buf, "vertex(%d) at %lf %lf", &vi, &a, &b);
        if (vi >= v_alloc) {
            v_alloc += ALLOC_VOR;
            v = (struct VorVertex*) G_realloc(v, v_alloc * sizeof(struct VorVertex));
        }
        v[vi].x = a;
        v[vi].y = b;
        break;
    case 'e':
        sscanf (buf, "e %d %d %d", &lineIndex, &vLeft, &vRight);
        if(vLeft == -1 || vRight == -1)
        {
            a = l[lineIndex].a;
            b = l[lineIndex].b;
            c = l[lineIndex].c;
            vIndex = (vLeft == -1) ? vRight : vLeft;
            knownPointAtLeft = (vLeft != -1);
            tmpx[0] = v[vIndex].x;
            tmpy[0] = v[vIndex].y;
            /* compute intersection with region's bounding box */
            if(!extend_line (window.south, window.north, window.west, window.east,
                         a, b, c, v[vIndex].x, v[vIndex].y, &tmpx[1], &tmpy[1],
                         knownPointAtLeft))
                break;
        } else {
            tmpx[0] = v[vLeft].x;
            tmpy[0] = v[vLeft].y;
            tmpx[1] = v[vRight].x;
            tmpy[1] = v[vRight].y;
            if(!in_region(v[vLeft].x, v[vLeft].y) || !in_region(v[vRight].x, v[vRight].y))
                D_clip( window.south, window.north, window.west, window.east,
                        &tmpx[0], &tmpy[0], &tmpx[1], &tmpy[1]);
        }
        /* check for boundary intersection and add points to boundary arrays as needed */
        handleBoundaryPoint(tmpx[0], tmpy[0]);
        handleBoundaryPoint(tmpx[1], tmpy[1]);
        if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
            G_fatal_error ("Out of memory");
        Vect_write_line (Map, AREA, Points);
        break;
    default:
        break;
    }
  }
	
  if(verbose) fprintf(stderr, "Sorting boundary rectangle crossings\n");

  /* output borders */
  outputBoundary(NORTH, Map, Points);
  outputBoundary(SOUTH, Map, Points);
  outputBoundary(EAST, Map, Points);
  outputBoundary(WEST, Map, Points);
  freeBoundaryArrays();

  G_free (v);
  G_free (l);
  Vect_destroy_line_struct (Points);
  fclose (ptr);
}


