#include "headers.h"

int triangulate = FALSE, sorted, plot, debug = FALSE, randmode, outputmode;
float xmin, xmax, ymin, ymax, deltax, deltay;

char execdir[80];

struct Site _sites[MAXSITES], *sites = _sites;

int nsites;
int siteidx;
int sqrt_nsites;
int nvertices;
struct Freelist sfl;
struct Site *bottomsite;

int nedges;
struct Freelist efl;

struct Freelist hfl;
struct Halfedge *ELleftend, *ELrightend;
int ELhashsize;
struct Halfedge **ELhash;

int PQhashsize;
struct Halfedge *PQhash;
int PQcount;
int PQmin;

/* sort sites on y, then x, coord */
int scomp (s1, s2)
  struct Point *s1, *s2;
{
  if (s1->y < s2->y)
    return (-1);
  if (s1->y > s2->y)
    return (1);
  if (s1->x < s2->x)
    return (-1);
  if (s1->x > s2->x)
    return (1);
  return (0);
}

/* sort GLsites on y, then x, coord */
int GLscomp (s1, s2)
  VERT *s1, *s2;
{
  if (s1->y < s2->y)
    return (-1);
  if (s1->y > s2->y)
    return (1);
  if (s1->x < s2->x)
    return (-1);
  if (s1->x > s2->x)
    return (1);
  return (0);
}

/* return a single in-storage site */
struct Site *
 nextone ()
{
  if (siteidx < nsites)
    return (&sites[siteidx++]);
  else
    return ((struct Site *) NULL);
}

sortGLsites ()
{
  float vx, vy;
  int k;
  char *a = NULL;
  float randr (), dx, dy;

  qsort ((char *) GLsites, nsites, sizeof (VERT), GLscomp);

  /* check: are there coincident points? */
  for (k = 1; k < nsites; k++)
    if ((GLsites[k - 1].y == GLsites[k].y) && (GLsites[k - 1].x == GLsites[k].x))
    {
      /* printf ("coincident sites at %d, %d!\n", k-1, k); */
      dx = GLsites[k].x * (1.0 / 4096.0);
      dy = GLsites[k].y * (1.0 / 4096.0);
      /* hack: jitter the last point randomly in its last significant digit */
      GLsites[k - 1].x += randr (-dx, dx);
      GLsites[k - 1].y += randr (-dy, dy);
    }
}

/*
 * implicit parameters: nsites, sqrt_nsites, xmin, xmax, ymin, ymax, deltax,
 * deltay (can all be estimates). Performance suffers if they are wrong;
 * better to make nsites, deltax, and deltay too big than too small.  (?)
 */
voronoi (nextsite)
  struct Site *(*nextsite) ();
{
  struct Site *newsite, *bot, *top, *temp, *p;
  struct Site *v;
  struct Point newintstar;
  int pm;
  struct Halfedge *lbnd, *rbnd, *llbnd, *rrbnd, *bisector;
  struct Edge *e;

  myfreeall ();

  if (nsites <= 1)
    return;

  freeinit (&sfl, sizeof (struct Site));

  /* bboxinit();   /* copies static array into nsites */
  geominit ();			/* internal use of deltax, deltay  */

  PQinitialize ();
  bottomsite = (*nextsite) ();
  out_site (bottomsite);
  ELinitialize ();

  newsite = (*nextsite) ();
  while (1)
  {
    if (!PQempty ())
      newintstar = PQ_min ();
    /* new site is smallest */
    if (newsite != (struct Site *) NULL && (PQempty ()
					  || newsite->coord.y < newintstar.y
					|| (newsite->coord.y == newintstar.y
				       && newsite->coord.x < newintstar.x)))
    {
      out_site (newsite);
      lbnd = ELleftbnd (&(newsite->coord));
      rbnd = ELright (lbnd);
      bot = rightreg (lbnd);
      e = bisect (bot, newsite);
      bisector = HEcreate (e, le);
      ELinsert (lbnd, bisector);
      if ((p = intersect (lbnd, bisector)) != (struct Site *) NULL)
      {
	PQdelete (lbnd);
	PQinsert (lbnd, p, dist (p, newsite));
      }
      lbnd = bisector;
      bisector = HEcreate (e, re);
      ELinsert (lbnd, bisector);
      if ((p = intersect (bisector, rbnd)) != (struct Site *) NULL)
      {
	PQinsert (bisector, p, dist (p, newsite));
      }
      newsite = (*nextsite) ();
    }
    else if (!PQempty ())
    {
      /* intersection is smallest */
      lbnd = PQextractmin ();
      llbnd = ELleft (lbnd);
      rbnd = ELright (lbnd);
      rrbnd = ELright (rbnd);
      bot = leftreg (lbnd);
      top = rightreg (rbnd);
      out_triple (bot, top, rightreg (lbnd));
      v = lbnd->vertex;
      makevertex (v);
      v_endpoint (lbnd->ELedge, lbnd->ELpm, v);
      v_endpoint (rbnd->ELedge, rbnd->ELpm, v);
      ELdelete (lbnd);
      PQdelete (rbnd);
      ELdelete (rbnd);
      pm = le;
      if (bot->coord.y > top->coord.y)
      {
	temp = bot;
	bot = top;
	top = temp;
	pm = re;
      }
      e = bisect (bot, top);
      bisector = HEcreate (e, pm);
      ELinsert (llbnd, bisector);
      v_endpoint (e, re - pm, v);
      deref (v);
      if ((p = intersect (llbnd, bisector)) != (struct Site *) NULL)
      {
	PQdelete (llbnd);
	PQinsert (llbnd, p, dist (p, bot));
      }
      if ((p = intersect (bisector, rrbnd)) != (struct Site *) NULL)
	PQinsert (bisector, p, dist (p, bot));
    }
    else
      break;
  }

  for (lbnd = ELright (ELleftend); lbnd != ELrightend; lbnd = ELright (lbnd))
  {
    e = lbnd->ELedge;
    out_ep (e);
  }
}
