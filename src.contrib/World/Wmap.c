# include <stdio.h>
# include "map.h"
# define NTRACK 10
# define AXMAX 1248
# define AYMAX 1024

#ifdef IMAGEN
#define linemod(x)  fprintf (stderr, "%s\n", x);/*nop x */
#endif IMAGEN
double atof (), floor (), ceil () ;

/*
 * int (*projection     )()                       ptr to func returning int
 * int (*azequidistant())()        func returning ptr to func returning int
 * int (* (*prog)     ())() ptr to func returning ptr to func returning int
 *
 */

int (*projection)() ;
int (*azequidistant())(),
    (*mercator())(),
    (*cylindrical())(),
    (*rectangular())(),
    (*orthographic())(),
    (*sinusoidal())(),
    (*azequalarea())(),
    (*stereographic())(),
    (*gnomonic())(),
    (*perspective())(),
    (*cylequalarea())(),
    (*conic())(),
    (*polyconic())(),
    (*bonne())(),
    (*lambert())(),
    (*albers())() ;

struct projs { char *name, *parms ;  int (*(*prog)())() ;  int npar, cut ; } projs[] =
{
"albers",        "latitude-1, latitude-2",            albers,        2, 1,
"azequalarea",   "no parms",                          azequalarea,   0, 0,
"azequidistant", "no parms",                          azequidistant, 0, 0,
"bonne",         "latitude",                          bonne,         1, 1,
"conic",         "latitude",                          conic,         1, 1,
"cylequalarea",  "latitude (<=89)",                   cylequalarea,  1, 1,
"cylindrical",   "no parms",                          cylindrical,   0, 1,
"gnomonic",      "no parms",                          gnomonic,      0, 0,
"lambert",       "latitude-1, latitude-2 (<=89.5)",   lambert,       2, 1,
"mercator",      "no parms",                          mercator,      0, 1,
"orthographic",  "no parms",                          orthographic,  0, 0,
"perspective",   "radius (abs(r - 1.)>=.01)",         perspective,   1, 0,
"polyconic",     "no parms",                          polyconic,     0, 1,
"rectangular",   "no parms",                          rectangular,   0, 1,
"sinusoidal",    "no parms",                          sinusoidal,    0, 1,
"stereographic", "no parms",                          stereographic, 0, 0,
0,               "",                                  0,             0, 0
} ;

char menu[] =
{
"\n\
options:\n\
-m			don't plot map\n\
-g			don't plot grid\n\
-s			don't erase screen before plotting\n\
-g n			plot n x n degree grid\n\
-g lat lon		plot lat x lon degree grid\n\
-t file			plot satellite track (dotted line)\n\
-T Title		plot a title for picture\n\
-u file			plot satellite track (solid line)\n\
-d n			plot only every n-th point\n\
-o lat lon twist	re-orient globe before plot\n\
-l bot top right left	set pre-transform window limits\n\
-w bot top right left	set post-transform window limits\n"
} ;

int cut ;
int delta = 1 ;
float params[2] ;
float orientation[3] = { 90., 0., 0. } ;  /* lat lon twist */
float limits[4]      = { -90., 90., -180., 180. } ;  /* lolat hilat lolon hilon */
float window[4]      = { -90., 90., -180., 180. } ;  /* lolat hilat lolon hilon */
float grid[2]        = { 10., 10. } ;  /* lat lon */
float lolat, hilat, lolon, hilon ;
float xrange, yrange, xmin = 100., xmax = -100., ymin = 100., ymax = -100. ;
int left, right, bottom, top ;
int sflag, mflag ;
float scale ;
struct track { int tracktyp ;  char *tracknam ; } track[NTRACK] ;
int ntrack ;
char *title = 0;

main (argc,argv)
char **argv ;
{
int i, j, k ;
int conn ;
char *s, *t ;
float x, y, lat, lon ;
int ix, iy ;
struct place g ;
char buffer[1024] ;

	if (argc <= 1) 
	{
		sprintf (buffer, "usage: %s projection params options or %s <help>",
			argv[0], argv[0]) ;
		error (buffer) ;
	}

	if ((k = keycmp (argv[1], projs)) == -1)
		{
		printf ("projections:\n") ;
		for (i = 0 ; projs[i].name ; i++) 
			printf ("%s (%s)\n", projs[i].name, projs[i].parms) ;
		if (strcmp (argv[1], "help") == 0)
			printf (menu) ;
		exit () ;
		}

	argv += 2 ;  argc -= 2 ;
	cut = projs[k].cut ;
	for (i = 0 ; i < projs[k].npar ; i++)
		{
		if (i >= argc || option (argv[i]))
			{
			printf ("%s needs %d params\n", projs[k].name, projs[k].npar) ;
			exit () ;
			}
		params[i] = atof (argv[i]) ;
		}
	argv += i ;  argc -= i ;

	while (argc > 0 && option (argv[0]))
		{
		argc-- ;  argv++ ;

		switch (argv[-1][1])
			{
			case 'm': mflag++ ;  break ;

			case 'g':
				for (i = 0 ; i < 2 && argc > i && ! option (argv[i]) ; i++)
					grid[1] = grid[i] = atof (argv[i]) ;
				if (i == 0)
					grid[0] = grid[1] = 0. ;
				argc -= i ;  argv += i ;
				break ;

			case 't':
			case 'u':
				for (i = 0 ; ntrack < NTRACK && argc > i && ! option (argv[i]) ; i++)
					{
					track[ntrack].tracktyp = argv[-1][1] ;
					track[ntrack++].tracknam = argv[i] ;
					}
				argc -= i ;  argv +=i ;
				break ;

			case 's': sflag++ ;  break ;

			case 'o':
				for (i = 0 ; i < 3 && i < argc && ! option (argv[i]) ; i++)
					orientation[i] = atof (argv[i]) ;
				argv += i ;  argc -= i ;
				break ;

			case 'l':
				for (i = 0 ; i < argc && i < 4 && ! option (argv[i]) ; i++)
					limits[i] = atof (argv[i]) ;
				argv += i ;  argc -= i ;
				break ;

			case 'd':
				if (argc > 0 && ! option (argv[0]))
					{
					delta = atoi (argv[0]) ;
					argv++ ;  argc-- ;
					}
				break ;

			case 'w':
				for (i = 0 ; i < argc && i < 4 && ! option (argv[i]) ; i++)
					window[i] = atof (argv[i]) ;
				argv += i ;  argc -= i ;
				break ;
			case 'T':
				if (argc > 0 )
					{
					title = argv[0];
					argv++ ;  argc-- ;
					}
				break ;
			}
		}

	if (argc > 0)
		error ("error in arguments") ;

	window[0] -= .01 ;  window[1] += .01 ;
	window[2] -= .01 ;  window[3] += .01 ;
	if ( window[0] >= window[1] || window[2] >= window[3] ||
             window[0] >   90.      || window[1] <  -90.      ||
             window[2] >  180.      || window[3] < -180.        )
	   error ("unreasonable window") ;
	for (i = 0 ; i < 4 ; i++)
	    window[i] *= RAD ;

	orient (orientation[0], orientation[1], orientation[2]) ;
	projection = (*projs[k].prog) (params[0], params[1]) ;
	if (projection == 0)
	   error ("unreasonable parameters") ;

	lolat = floor (limits[0] / 10) * 10 ;
	hilat = ceil  (limits[1] / 10) * 10 ;
	lolon = floor (limits[2] / 10) * 10 ;
	hilon = ceil  (limits[3] / 10) * 10 ;

	if (lolon > hilon)
		hilon += 360. ;
	if (lolon >= hilon || lolat >= hilat || lolat < -90. || hilat > 90.)
		error ("unreasonable limits") ;

	for (lat = lolat ; lat <= hilat ; lat += 10.)
	    for (lon = lolon ; lon <= hilon ; lon += 10.)
		{
		if (normproj (lat, lon, &x, &y) <= 0)
		   continue ;
		if (x < xmin) xmin = x ;   if (x > xmax) xmax = x ;
		if (y < ymin) ymin = y ;   if (y > ymax) ymax = y ;
		}

	xrange = xmax - xmin ;
	yrange = ymax - ymin ;
	if (xrange / AXMAX > yrange / AYMAX)
		scale = AXMAX / (xrange * 1.1) ;
	else    scale = AYMAX / (yrange * 1.1) ;
/*
printf ("minmax %.2f %.2f %.2f %.2f\n", xmin, xmax, ymin, ymax) ;
*/

	openpl () ;
	left   = (xmin - .05 * xrange) * scale ;  right  = left + AXMAX ;
	bottom = (ymin - .05 * yrange) * scale ;  top    = bottom + AYMAX ;
	space (left, top, right, bottom) ;
	if ( ! sflag)
		erase () ;
	if (title) {
		linemod ("yellow");
		move (left-10, top-10);
		/*
		label (title);
		*/
	}


	linemod ("green") ;   /* grid */
	if (grid[0] > 0.)
	   for (lat = ceil (lolat / grid[0]) * grid[0] ; lat <= hilat ; lat += grid[0])
	       for (conn = 0, lon = lolon ; lon <= hilon ; lon += 2)
	           { deg2rad (lat, &g.nlat) ;
		     deg2rad (lon, &g.wlon) ;
	             conn = plotpt (&g, conn) ; }

	linemod ("blue") ;   /* grid */
	if (grid[1] > 0.)
	   for (lon = ceil (lolon / grid[1]) * grid[1] ; lon <= hilon ; lon += grid[1])
	       for (conn = 0, lat = lolat ; lat <= hilat ; lat += 2)
	           { deg2rad (lat, &g.nlat) ;
	             deg2rad (lon, &g.wlon) ;
	             conn = plotpt (&g, conn) ; }

	linemod ("white") ;  /* land */
	if ( ! mflag)
		dodata () ;
	for (i = 0 ; i < ntrack ; i++)
		satellite (&track[i]) ;

	move (right, bottom) ;
	closepl () ;
}

normproj (lat, lon, x, y) float lat, lon ; float *x, *y ;
{
	int i ;
	struct place geog, map ;
	deg2rad (lat, &geog.nlat) ;
	deg2rad (lon, &geog.wlon) ;
/*
	printp (&geog) ;
*/
	normalize (&geog) ;
	if ( ! inwindow (&geog))
		return (-1) ;
	i = (*projection) (&geog, x, y) ;
/*
	printp (&geog) ;
	printf ("%d %.3f %.3f\n", i, *x, *y) ;
*/
	return (i) ;
}

inwindow (geog) struct place *geog ;
{
if (geog->nlat.l < window[0] || geog->nlat.l > window[1] ||
    geog->wlon.l < window[2] || geog->wlon.l > window[3])
	return (0) ;
   else return (1) ;
}

option (s) char *s ;
{
if (s[0]=='-' && (s[1] < '0'||s[1] > '9')  && s[1] != '.')
	return (1) ;
   else return (0) ;
}

struct {int hi, lo ; } ;
FILE *WORLD ;
long patch[18][36] ;

dodata ()
{
short int n, kx, ky ;
int k, ip, jp, i, j, conn ;
struct place g ;
long longint ;
float x, y, lat, lon ;

if (freopen ("usr/dict/world.x", "r", stdin) == NULL)
	error ("can't find map index") ;
if ((WORLD = fopen ("usr/dict/world.new", "r")) == NULL)
	error ("can't find map data") ;

while ( scanf ("%d%d%D", &i, &j, &longint) == 3 )
	patch[i + 9][j + 18] = longint ;

for (lat = lolat ; lat < hilat ; lat += 10.)
    for (lon = lolon ; lon < hilon ; lon += 10.)
        {
        if ( ! (normproj (lat,      lon,      &x, &y) > 0 ||
	        normproj (lat + 10, lon,      &x, &y) > 0 ||
	        normproj (lat,      lon + 10, &x, &y) > 0 ||
	        normproj (lat + 10, lon + 10, &x, &y) > 0) ) continue ;

	i = pnorm (lat) ;  j = pnorm (lon) ;
	if (patch[i + 9][j + 18] & 1)
		continue ;
	fseek (WORLD, patch[i + 9][j + 18], 0) ;
	while ((ip = getc (WORLD)) != EOF && (jp = getc (WORLD)) != EOF)
	      {
	      if (ip != (i & 0xFF) || jp != (j & 0xFF))
		 break ;
	      fread (&n, sizeof (short int), 1, WORLD) ;
	      for (conn = 0, k = 0 ; k < n ; k++)
		  {
		  fread (&kx, sizeof (short int), 1, WORLD) ;
		  fread (&ky, sizeof (short int), 1, WORLD) ;
		  if (k % delta != 0 && k != n - 1)
		     continue ;
		  conv (kx, &g.nlat) ;  conv (ky, &g.wlon) ;
		  conn = plotpt (&g, conn) ;
		  }
	      }
	}
}

satellite (t) struct track *t ;
{
int conn ;
float lat, lon ;
struct place place ;

if (freopen (t->tracknam, "r", stdin) == NULL)
	error ("can't find track") ;
linemod (t->tracktyp == 't' ? "red" : "yellow") ;

conn = 0 ;
while (scanf ("%f%f", &lat, &lon) == 2)
      {
      if (lat > 90.)
	      { conn = 0 ;  continue ; }
      deg2rad (lat, &place.nlat) ;  deg2rad (lon, &place.wlon) ;
      if (limits[0] * RAD > place.nlat.l || limits[1] * RAD < place.nlat.l ||
          limits[2] * RAD > place.wlon.l || limits[3] * RAD < place.wlon.l)
	      { conn = 0 ;  continue ; }
      conn = plotpt (&place, conn) ;
      }
}

pnorm (x) float x ;
{
int i ;

i = x / 10. ;  i %= 36 ;
if (i >= 18) return (i - 36) ;
if (i < -18) return (i + 36) ;
	     return (i) ;
}

conv (k, g) struct coord *g ;
{
g->l = .0001 * k ;  SinCos (g) ;
}

error (s) char *s ;
{
closepl () ;  fprintf (stderr, "\r\n%s\n", s) ;  exit () ;
}

int oldx, oldy ;

cpoint (xi, yi, conn)
{
if (xi < left   || xi >= right)
	return (0) ;
if (yi < bottom || yi >= top)
	return (0) ;

if (conn) { if (oldx != xi || oldy != yi)
	       line (oldx, oldy, xi, yi) ;
	    oldx = xi ;  oldy = yi ; }
   else	  { oldx = xi ;  oldy = yi ; }

return (1) ;
}

plotpt (g, conn) struct place *g ;
{
int kx, ky ;
float xs, ys ;
float x, y ;
static float olon ;

normalize (g) ;
if ( ! inwindow (g))
	return (0) ;
if (cut && conn && abs (g->wlon.l - olon) >= PI)
	conn = 0 ;
olon = g->wlon.l ;
if ( (*projection) (g, &x, &y) <= 0)
	return (0) ;

kx = x * scale ;
ky = y * scale ;

return (cpoint (kx, ky, conn)) ;
}

intabs (x)
{
return (x >= 0 ? x : -x) ;
}

keycmp (k, l) register char *k ;  struct projs *l ;
{
register struct projs *p, *matched ;
register int kl = strlen (k), matches = 0 ;

for ( p = l ; p->name ; p++ )
    if (strcmp (k, p->name) == 0) return (p - l) ;
       else if (strncmp (k, p->name, kl) == 0)
               { matches++ ;  matched = p ; }

if (matches == 1) return (matched - l) ;
   else return (-1) ;
}
