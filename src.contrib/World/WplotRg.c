# include <stdio.h>
# include <math.h>
#define Db 


/*
 * Header file for RETRO-GRAPHICS equipped ADM-3A (RG-512)
 *   ...simulates TEKTRONIX 4010 (almost)  P.E.Kane 1980
 */

/*
 * These are the sizes of the actual RG-512 graphics grid
 */
#define	XMIN	0
#define	XMAX	511
#define	XSIZE	(XMAX+1)
#define YMIN	0
#define	YMAX	249
#define	YSIZE	(YMAX+1)
/* 
 * And these are the sizes of the simulated TEKTRONIX grid
 */
#define	TXMIN	0
#define	TXMAX	1023
#define	TXSIZE	(TXMAX+1)
#define	TYMIN	0
#define	TYMAX	779
#define	TYSIZE	(TYMAX+1)
#define	TCX	(TXSIZE/2)
#define	TCY	(TYSIZE/2)
/*
 * Since the X-10 option is standard the Z-80A inside the
 * RG-512 will scale the grid coordinates using the following
 * relations:	x' = x/2,  y' = y(82/256)
 * where: 	x', y' are the actual grid coordinates
 *		x , y  are the given (simulated) coordinates
 */
#define	XFUDGE	(2)
#define	YFUDGE	(82/256)

/*
 * Defines for RG-512 with x-10 option
 */
#define	US	037	/* enter 4010 alpha mode */
#define	CAN	030	/* enter ADM-3A alpha mode */
#define GS	035	/* enter Vector mode */
#define FS	034	/* enter Point mode */
#define	ESC	033	/* escape */
#define DEL	0177	/* */
#define	FF	014	/* */
#define	EM	031	/* Clear graphics mem */
/*
 * Mode Transition
 */
mode (m)  register char m;
{
	switch (m)
	{
	case 'A':		/* ADM-3A Alpha Mode */
		outc (US); outc (CAN); break;
	case 'a':		/* 4010 Alpha Mode */
		outc (US); break;
	case 'v':		/* Vector mode */
		outc (GS); break;
	case 'p':		/* Point mode */
		outc (FS); break;
	default:
		fprintf (stderr, "Bad mode %c (%o)\n", m, m);
	}
	return;
}
	
int  Xoffset, Yoffset, Xscale, Yscale, dotmode;
/*
 * Output an X/Y coordinate pair
 */
outxy (x,y)	 register short x, y;
{
	register short hy, ly, hx, lx;
/*printf ("x=%d, y=%d\n", x,y); return; */

 	x /= Xscale;	y /= Yscale;
	x += Xoffset;	y += Yoffset;
	hy = 0040 | ((y & 01740) >> 5);
	ly = 0140 | (y & 0037);
	hx = 0040 | ((x & 01740) >> 5);
	lx = 0100 | (x & 0037);
	outc (hy); outc (ly); 
	outc (hx); outc (lx);

	return;
}

/*
 * Set data level
 */
datalevel (c)	 register char c;
{
	switch (c)
	{
	case 'w':		/* White */
		outc (ESC); outc ('a'); break;
	case 'b':		/* Black */
		outc (ESC); outc (DEL); break;
	default:
		fprintf (stderr, "Bad data level %c (%o)\n", c, c);
		break;
	}
	return;
}

/*
 * Put a char to output
 */
outc (c)	 register short c;
{
	fprintf (stdout,"%c", c);
}

/*
 * Plot a point at x,y with data level d
 */
point (x,y,d)	 register short x, y; register  char d;
{
	mode ('p');	/*don't need to do this if in point mode already */
	datalevel (d);
	outxy (x, y);
	return;
}

/*
 * Draw a vector from fx, fy  to  tx, ty  with data level d
 */
vector (fx,fy,tx,ty,d)	 register short fx, fy, tx, ty; register char d;
{
	mode ('v');
	/*datalevel (d); */
	outxy (fx, fy);
	outxy (tx, ty);	/* Note: if  high y, low y, high x have
			 *       not changed they do not need to
			 *       be output 
	     		 */
	return;
}

clear ()
{
	outc (GS);
	outc (EM);
	fflush (stdout);
	sleep (1);
}

/*
 * Plot interface
 */

static int color = 1;

line (x1, y1, x2, y2)
{ Db ("line (%dx, %dy, %dx, %dy)\n", x1, y1, x2, y2) ;
  if (!dotmode ||  color % dotmode == 1 ) vector (x1, y1, x2, y2, 'w');
  if ( dotmode ) color++ ;
}

space (l, b, r, t)
{ Db ("space (%dl, %db, %dr, %dt)\n", l, b, r, t) ;
  Xoffset =  -l;  Yoffset = -b;
  Xscale =  (r-l) / TXSIZE;
  Yscale =  (t-b) / TYSIZE;
 }

linemod (s) char *s;
{ Db ("linemod (%s)\n", s) ;
  if (strcmp (s, "dot") == 0) dotmode = 3;
  else if (strcmp (s, "dotdash") == 0)  dotmode = 2;
  else dotmode = 0;
  color = 1;
}

move (x, y)
{ Db ("move (%dx, %dy)\n", x, y) ;
  datalevel ('b');
  mode ('v');
  outxy (x, y);
}

erase ()
{ Db ("erase()\n") ;
  clear ();
}

openpl ()
{ Db ("openpl()\n") ; 
  mode ('a');
  datalevel ('w');
}

closepl ()
{ Db ("closepl()\n") ; 
  mode ('A');
}

