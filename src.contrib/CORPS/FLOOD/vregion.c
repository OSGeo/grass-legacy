#include "headers.h"

#define LBND 0
#define BBND 1
#define RBND 2
#define TBND 3

/* left, right, top bottom */
#define NBOUND(x,y) (x >= Pxmax ? RBND : (x <= Pxmin ? LBND : (y >= Pymax ? TBND : (y <= Pymin ? BBND : -1))))

#define BBOX_THERE() ((pxmin == txmin) && (pxmax == txmax) && (pymin == tymin) && (pymax && pymax))
#define BBOX_NOT_THERE() (!BBOX_THERE())

static float Pxmin, Pxmax, Pymin, Pymax;	/* floating point tolerance
						 * values */
static float pxmin = 1E10, pxmax = -1E10, pymin = 1E10, pymax = -1E10;

/* space for theta after x*y* */
static float xminymin[3], xminymax[3], xmaxymax[3], xmaxymin[3];

/* these point to the x*y* */
static VERTTHETA *corner[4];

static POINT v, v1, v2, v3, v4;

static int numverts = 0, numvedges = 0, numtris;

/* sites[i].coord.x,y defined and sorted in main() */

VERT GLsites[MAXVERTS];
static VERT verts[MAXVERTS];
static EDGE vedges[MAXEDGES];
static TRI tris[MAXTRIS];
static int inverse[MAXVERTS];

float randr (low, high)
  float low, high;
{
  float value;

  value = low + (rand () / ((1 << 15) - 1.0)) * (high - low);
  return (value);
}

out_site (s)
  struct Site *s;
{
}

out_bisector (e)
  struct Edge *e;
{
}

out_ep (e)
  struct Edge *e;
{
  if (!triangulate)
    clip_line (e);

  if (debug)
  {
    printf ("out_ep(): edge %d", e->edgenbr);
    printf (" ep %d", e->ep[le] != (struct Site *) NULL ? e->ep[le]->sitenbr : -1);
    printf (" ep %d", e->ep[re] != (struct Site *) NULL ? e->ep[re]->sitenbr : -1);
    printf (" reg %d", e->reg[le] != (struct Site *) NULL ? e->reg[le]->sitenbr : -1);
    printf (" reg %d\n", e->reg[re] != (struct Site *) NULL ? e->reg[re]->sitenbr : -1);
  }
}

out_vertex (v)
  struct Site *v;
{
  if (!triangulate)
  {
    verts[numverts].x = v->coord.x;
    verts[numverts].y = v->coord.y;
    if (numverts < MAXVERTS)
      numverts++;
    else
    {
      fprintf (stderr, "\nvert list overflow!");
      exit (-1);
    }
  }

  if (debug)
    printf ("vertex(%d) at %10.7f %10.7f\n", v->sitenbr, v->coord.x, v->coord.y);
}

out_triple (s1, s2, s3)
  struct Site *s1, *s2, *s3;
{
  if (triangulate)
  {
    tris[numtris].v1 = (VERT *) & (sites[s1->sitenbr].coord);
    tris[numtris].v2 = (VERT *) & (sites[s2->sitenbr].coord);
    tris[numtris].v3 = (VERT *) & (sites[s3->sitenbr].coord);

    if (numtris < MAXTRIS)
      numtris++;
    else
    {
      fprintf (stderr, "out_triple(): triangle list overflow!\n");
      exit (-1);
    }
  }
}

vdinit ()
{
  numverts = numvedges = numtris = 0;
}

/* here, want to copy the gl sites INTO the voronoi array */
/* gl sites are written exactly once at beginning of time */
bboxinit ()
{
  int k;
  float dx, dy, x, y;

  /* get tight bounding box */
  xmin = 1e10;
  xmax = -1e10;

  ymin = 1e10;
  ymax = -1e10;

  for (k = 0; k < nsites; k++)
  {
    x = sites[k].coord.x = GLsites[k].x;
    y = sites[k].coord.y = GLsites[k].y;

    sites[k].refcnt = 0;
    sites[k].sitenbr = k;

    if (x < xmin)
      xmin = x;
    if (y < ymin)
      ymin = y;

    if (x > xmax)
      xmax = x;
    if (y > ymax)
      ymax = y;
  }

  /* NOW: xmin, ymin, xmax, ymax EXACT, as required by voronoi() */
  /* we shall fool with pxmin, pymin, pxmax, pymax, as used by clip() */

#define EPSILON 1.0

  /* compute 'loose' bounding box */
  dx = xmax - xmin;
  dx = MAX (dx, EPSILON);

  dy = ymax - ymin;
  dy = MAX (dy, EPSILON);

  pxmin = xmin - dx * 0.25;
  pymin = ymin - dy * 0.25;

  pxmax = xmax + dx * 0.25;
  pymax = ymax + dy * 0.25;

#ifdef STUPID_C_COMPILER
  printf ("/* xmin, ymin %10.7f %10.7f; xmax, ymax %10.7f %10.7f; */\n", xmin, ymin, xmax, ymax);
  printf ("/* pxmin, pymin %10.7f %10.7f; pxmax, pymax %10.7f %10.7f; crad %10.7f; */\n", pxmin, pymin, pxmax, pymax, cradius);
#endif
}

int clip_line (e)
  struct Edge *e;
{
  struct Site *s1, *s2;
  struct Site *r1, *r2;
  float x1, x2, y1, y2;

  if (e->a == 1.0 && e->b >= 0.0)
  {
    s1 = e->ep[1];
    s2 = e->ep[0];

    r1 = e->reg[1];
    r2 = e->reg[0];
  }
  else
  {
    s1 = e->ep[0];
    s2 = e->ep[1];

    r1 = e->reg[0];
    r2 = e->reg[1];
  }

  if (e->a == 1.0)
  {
    y1 = pymin;
    if (s1 != (struct Site *) NULL && s1->coord.y > pymin)
      y1 = s1->coord.y;
    if (y1 > pymax)
      return;
    x1 = e->c - e->b * y1;
    y2 = pymax;
    if (s2 != (struct Site *) NULL && s2->coord.y < pymax)
      y2 = s2->coord.y;
    if (y2 < pymin)
      return (0);
    x2 = e->c - e->b * y2;
    if ((x1 > pxmax && x2 > pxmax) || (x1 < pxmin && x2 < pxmin))
      return;
    if (x1 > pxmax)
    {
      x1 = pxmax;
      y1 = (e->c - x1) / e->b;
    }
    if (x1 < pxmin)
    {
      x1 = pxmin;
      y1 = (e->c - x1) / e->b;
    }
    if (x2 > pxmax)
    {
      x2 = pxmax;
      y2 = (e->c - x2) / e->b;
    }
    if (x2 < pxmin)
    {
      x2 = pxmin;
      y2 = (e->c - x2) / e->b;
    }
  }
  else
  {
    x1 = pxmin;
    if (s1 != (struct Site *) NULL && s1->coord.x > pxmin)
      x1 = s1->coord.x;
    if (x1 > pxmax)
      return (0);
    y1 = e->c - e->a * x1;
    x2 = pxmax;
    if (s2 != (struct Site *) NULL && s2->coord.x < pxmax)
      x2 = s2->coord.x;
    if (x2 < pxmin)
      return (0);
    y2 = e->c - e->a * x2;
    if ((y1 > pymax && y2 > pymax) || (y1 < pymin && y2 < pymin))
      return (0);
    if (y1 > pymax)
    {
      y1 = pymax;
      x1 = (e->c - y1) / e->a;
    }
    if (y1 < pymin)
    {
      y1 = pymin;
      x1 = (e->c - y1) / e->a;
    }
    if (y2 > pymax)
    {
      y2 = pymax;
      x2 = (e->c - y2) / e->a;
    }
    if (y2 < pymin)
    {
      y2 = pymin;
      x2 = (e->c - y2) / e->a;
    }
  }

  /*
   * fprintf (stderr, "clip_line(): edge %d is (%10.7f, %10.7f) to (%10.7f,
   * %10.7f)\n", numvedges, x1, y1, x2, y2);
   */

  if (!triangulate)
  {
    vedges[numvedges].x1 = x1;
    vedges[numvedges].y1 = y1;

    vedges[numvedges].x2 = x2;
    vedges[numvedges].y2 = y2;

    vedges[numvedges].nbr1 = (r1 != NULL ? r1->sitenbr : -9998);
    vedges[numvedges].nbr2 = (r2 != NULL ? r2->sitenbr : -9999);

    if (r1 != NULL && r2 != NULL)
    {
      vedges[numvedges].xm = AVG (r1->coord.x, r2->coord.x);
      vedges[numvedges].ym = AVG (r1->coord.y, r2->coord.y);
    }

    if (debug)
      printf ("clip_line puts edge induced by %d and %d\n", r1->sitenbr, r2->sitenbr);

    if (numvedges < MAXEDGES)
      numvedges++;
    else
    {
      fprintf (stderr, "clip_line(): edge list overflow!\n");
      exit (-1);
    }
  }
}

/*
 * load_vsites(): accept the n voronoi sites (x_n, y_n) calculate and store
 * the voronoi diagram over the n sites, clipping all infinite edges to bbox:
 * [xmin, ymin, xmax, ymax].
 * 
 * note: if (xmin,ymin,xmax,ymax are all == 0.0), OR if these do not enclose the
 * data, a bounding box will be computed over the input.
 * 
 * returns: -1 if error 0 otherwise
 */
int
 load_vsites (n, usites, uxmin, uymin, uxmax, uymax)
  int n;
  float **usites;		/* float usites[][2];	 *//* sites in x,y
				 * order */
  float uxmin, uymin, uxmax, uymax;
{
  int k, compute_bbox, sid, tid;
  float dx, dy, x, y;

  if (n >= MAXSITES)
  {
    fprintf (stderr, "load_vsites(): can't handle >= %d sites.\n", MAXSITES);
    return (-1);
  }

  compute_bbox = (uxmin == 0.0) && (uymin == 0.0) && (uxmax == 0.0) && (uymax == 0.0);

  /* copy the sites into GLsites and set global nsites */
  for (k = 0; k < n; k++)
  {
    GLsites[k].x = usites[k][0];
    GLsites[k].y = usites[k][1];
    GLsites[k].pid = k;
  }

  nsites = n;

  /* sort GL sites lexicographically by position */
  sortGLsites ();

  /* copy sorted GLsites into voronoi alg sites */
  bboxinit ();

  /*
   * now, if user punted on bbox calculation, OR if user bbox does not truly
   * enclose user data, we use our bbox (computed in initbbox).  otherwise we
   * take the user's.
   */
  if (!(compute_bbox || uxmin > xmin || uymin > ymin || uxmax < xmax || uymax < ymax))
  {
    pxmin = uxmin;
    pymin = uymin;

    pxmax = uxmax;
    pymax = uymax;
  }

  xminymax[0] = xminymin[0] = pxmin;
  xminymin[1] = xmaxymin[1] = pymin;

  xmaxymax[0] = xmaxymin[0] = pxmax;
  xmaxymax[1] = xminymax[1] = pymax;

  corner[0] = (VERTTHETA *) xminymin;
  corner[1] = (VERTTHETA *) xmaxymin;
  corner[2] = (VERTTHETA *) xmaxymax;
  corner[3] = (VERTTHETA *) xminymax;

  /*
   * now: set the floating point tolerance values P*** to be 1 or 2
   * significant bits inside the p***
   */
  /*
   * be careful to use RELATIVE error; that is, to scale the tolerance to the
   * bbox values themselves
   */
  /*
   * now, if some user puts points way out in left field, our technique
   * handles the ranges correctly
   */
  {
    float dx = (pxmax - pxmin) * (1.0 / (4096.0));	/* twelve binary digits
							 * out */
    float dy = (pymax - pymin) * (1.0 / (4096.0));	/* twelve binary digits
							 * out */

    Pxmin = pxmin + dx;
    Pxmax = pxmax - dx;

    Pymin = pymin + dy;
    Pymax = pymax - dy;
  }

  /* compute inverse of external->internal sid mapping */
  for (sid = 0; sid < nsites; sid++)
    inverse[sid] = GLsites[sid].pid;

  /* zero list lengths out */
  vdinit ();

  /* run the voronoi code, no triangulate */
  triangulate = FALSE;
  voronoi (nextone);

  /* RE-copy sorted GLsites into voronoi alg sites */
  bboxinit ();

  /* run the voronoi code, do triangulate */
  triangulate = TRUE;
  voronoi (nextone);

  /* invert the integer id's in sites[], for find_dtriangles() */
  /* and restore the original vertex values (from GLsites)	 */
  for (sid = 0; sid < nsites; sid++)
  {
    sites[sid].sitenbr = GLsites[sid].pid;
    sites[sid].coord.x = GLsites[sid].x;
    sites[sid].coord.y = GLsites[sid].y;
  }

  return (0);
}

static VERTTHETA vtlist[1024], slist[1024];
static int vtnum;

int
 vtcomp (vt1, vt2)
  VERTTHETA *vt1, *vt2;
{
  if (vt1->theta < vt2->theta)
    return (-1);
  else if (vt1->theta > vt2->theta)
    return (1);
  else
    return (0);
}

/*
 * find_vregion(sid, plen, pverts) given a site id 'sid' from 0..nsites-1
 * inclusive, returns the voronoi polygon associated with that site in the
 * array 'pverts', and its length on call stack.
 * 
 * the vertices are returned in counterclockwise order.
 * 
 * returns: -1 if error condition plen > 2 [i.e., the # of verts in the voronoi
 * polygon] otherwise
 */
int
 find_vregion (vsid, pverts)
  int vsid;
  float pverts[][2];
{
  int sid, b, k, vnum, bnd1, bnd2, bdiff, sleft, lag, lead, p1a, p1b, p2a, p2b;
  float x1, y1, x2, y2, theta1, theta2, lasttheta, dtheta, h;

  /* note that PUTV(v,sid,vid) has the side effect of incrementing vid */
#define PUTV(v,sid,vid)	{ pverts[vid][0] = (v)->x; pverts[vid][1] = (v)->y; vid++; }

  if (vsid < 0 || vsid >= nsites)
  {
    fprintf (stderr, "find_vregion(%d) called with illegal site id.\n", vsid);
    return (-1);
  }

  /*
   * first thing is to convert user's 'virtual' site id to a 'physical' site
   * id, namely, an index into GLsites
   */
  for (sid = 0; sid < nsites; sid++)
    if (GLsites[sid].pid == vsid)
      break;

  if (sid == nsites)
  {
    fprintf (stderr, "find_vregion(%d) can't find requested site id.\n", vsid);
    return (-1);
  }

  for (k = 0; k < 4; k++)
    corner[k]->theta = atan2 (corner[k]->y - GLsites[sid].y, corner[k]->x - GLsites[sid].x);

  for (vtnum = 0, k = 0; k < numvedges; k++)
    if (vedges[k].nbr1 == sid || vedges[k].nbr2 == sid)
    {
      /*
       * add both ends of the edge, and their thetas, to unsorted list
       * (parent is edge k)
       */
      slist[vtnum].e1 = slist[vtnum].e2 = k;
      slist[vtnum].theta = atan2 (vedges[k].y1 - GLsites[sid].y, vedges[k].x1 - GLsites[sid].x);
      slist[vtnum].x = vedges[k].x1;
      slist[vtnum++].y = vedges[k].y1;

      slist[vtnum].e1 = slist[vtnum].e2 = k;
      slist[vtnum].theta = atan2 (vedges[k].y2 - GLsites[sid].y, vedges[k].x2 - GLsites[sid].x);
      slist[vtnum].x = vedges[k].x2;
      slist[vtnum++].y = vedges[k].y2;
    }

  /* now we have a list of vtnum thetas and vertices.  sort it on theta */
  qsort ((char *) slist, vtnum, sizeof (VERTTHETA), vtcomp);

  /* next, install the unique slist entries into vtlist */
  lag = lead = 0;
  vtlist[lag] = slist[lead];
  lasttheta = -10.0;

  while (lead < vtnum)
  {
    if (fabs (slist[lead].theta - lasttheta) > 1E-4)
    {
      lasttheta = slist[lead].theta;
      vtlist[lag++] = slist[lead++];
    }
    else
    {
      vtlist[lag - 1].e2 = slist[lead++].e1;
    }
  }

  vtnum = lag;
  /*
   * printf ("\n"); for (k = 0; k < vtnum; k++) printf ("vtlist[%d]: x,y
   * %f,%f; theta %f; edges %d %d\n", k, vtlist[k].x, vtlist[k].y,
   * vtlist[k].theta, vtlist[k].e1, vtlist[k].e2);
   */

  for (vnum = 0, k = 0; k < vtnum; k++)
  {
    if (fabs (vtlist[(k + vtnum - 1) % vtnum].theta - vtlist[k].theta) < 1E-4)
    {
      fprintf (stderr, "find_vregion(%d): vtlist %d, %d identical?\n", sid, (k + vtnum - 1) % vtnum, k);
      return (-1);
    }

    x1 = vtlist[(k + vtnum - 1) % vtnum].x;
    y1 = vtlist[(k + vtnum - 1) % vtnum].y;
    p1a = vtlist[(k + vtnum - 1) % vtnum].e1;
    p1b = vtlist[(k + vtnum - 1) % vtnum].e2;

    x2 = vtlist[k].x;
    y2 = vtlist[k].y;
    p2a = vtlist[k].e1;
    p2b = vtlist[k].e2;

    /* now: if the two vertices come from the same edge, output and continue */
    if (((p1a == p2a) || (p1a == p2b) || (p1b == p2a) || (p1b == p2b)) && (vtnum > 2))
    {
      PUTV (&vtlist[k], sid, vnum);
      continue;
    }

    /* otherwise must fill in missing corners between x1,y1 and x2,y2 */
    bnd1 = NBOUND (x1, y1);
    bnd2 = NBOUND (x2, y2);

    /* find number of CLOCKWISE steps around bbox */
    if (bnd1 >= 0 && bnd2 >= 0)
      bdiff = ((bnd2 - bnd1) + 4) % 4;	/* from 0 to 3 */
    else
      bdiff = 0;

    /* if they were on the same bounding box edge, output and continue */
    if (bdiff == 0)
    {
      PUTV (&vtlist[k], sid, vnum);
      continue;
    }

    /* special case: exactly two vertices */
    if (vtnum == 2)
    {
      theta1 = vtlist[0].theta;
      theta2 = vtlist[1].theta;
      dtheta = theta2 - theta1;

      /*
       * add all corners OUTSIDE the angular range of the edge as seen from
       * the site
       */
      for (b = 0; b < 4; b++)
      {
	if ((dtheta >= M_PI) && (corner[b]->theta > theta1) && (corner[b]->theta < theta2))
	  vtlist[vtnum++] = *corner[b];
	else if ((dtheta < M_PI) && ((corner[b]->theta < theta1) || (corner[b]->theta > theta2)))
	  vtlist[vtnum++] = *corner[b];
      }

      /* resort the (small) vertex list by theta */
      qsort ((char *) vtlist, vtnum, sizeof (VERTTHETA), vtcomp);

      /* and output */
      for (b = 0; b < vtnum; b++)
	PUTV (&vtlist[b], sid, vnum);

      break;
    }

    for (b = 0; b < bdiff; b++)
      PUTV (corner[(bnd1 + b) % 4], sid, vnum);

    PUTV (&vtlist[k], sid, vnum);
  }				/* k 0 ..vtnum */

  return (vnum);
}

/*
 * int find_dtriangles (**dtris)
 * 
 * returns: -1 if error condition, *dtris == NULL o/wise, # of delaunay
 * triangles, *dtris == array of TRIS (see voronoi.h)
 */
int
 find_dtriangles (dtris)
  TRI **dtris;
{
  if (numtris <= 0)
  {
    *dtris = NULL;
    return (-1);
  }
  else
  {
    *dtris = tris;
    return (numtris);
  }
}
/*============================================================================*/
find_adjacent( vsid, adjlist )
   int vsid;
   int *adjlist;
{
   int  sid, aid, vaid, nadj, k;
   extern float **vsite;
 
  if (vsid < 0 || vsid >= nsites)
  {
    fprintf (stderr, "find_adjacent(%d) called with illegal site id.\n", vsid);
    return (-1);
  }

  /*
   * first thing is to convert user's 'virtual' site id to a 'physical' site
   * id, namely, an index into GLsites
   */
  for (sid = 0; sid < nsites; sid++)
    if (GLsites[sid].pid == vsid)
      break;
 
  if (sid == nsites)
  {
    fprintf (stderr, "find_adjacent(%d) can't find requested site id.\n", vsid);    return (-1);
  }
 
  nadj = 0;
  for ( k = 0; k < numvedges; k++)
    if (vedges[k].nbr1 == sid || vedges[k].nbr2 == sid)
    {
      /*-----------------*/
      /* get adjacent id */
      /*-----------------*/
      if ( vedges[k].nbr1 == sid )
         aid = vedges[k].nbr2;
      else
         aid = vedges[k].nbr1;

      /*-----------------------------------------------*/
      /* convert adjacent id back to 'virtual' site id */
      /*-----------------------------------------------*/
      vaid = GLsites[aid].pid;
 
      /*-------------------------*/
      /* add adjacent id to list */
      /*-------------------------*/
      adjlist[nadj++] = vaid;
    }

   return( nadj );
}
