#ifndef lint
static char *SCCSID = "@(#)compress.c	AMG v.3.2";
#endif
/* convert lon-lat data to incremental mode */
# include "mapgen.h"
# include "coast.h"
# include <stdio.h>

# define PREC 32
# define BITS_BYTE 8
# define iabs(x) ((x) < 0 ? -(x) : (x))
# define isign(x, y) ((y) < 0 ? -(x) : (x))

static slog2(), analys(), cmprs(), range(), clr_lsbs(), reduce();
static long rshift();
static int	base_s;	/* shift factor reflecting data resolution */
static long	mask_s, round_s, last_loc;

static struct dircty wk;
	/* entry to establish resolution */
set_res(m_o_g) long m_o_g; {	/* meters on ground resolution */
	base_s = slog2( m_o_g *= CTS_METER) - 2;
	if (base_s < 0) base_s = 0;
	wk.cntrl = base_s;
	wk.location = 0;
	last_loc = 0;
	if (base_s) {
		mask_s = (-1L) << base_s;
		round_s = 1L << (base_s - 1);
	}
}
	struct dircty * /* conversion routine */
xypack(x, y, n) long *x, *y; int n; {
	int len;
		/* clean up longitude peculiarities */
	reduce(x, n);
		/* clear least significant bits */
	clr_lsbs(x, n);
	clr_lsbs(y, n);
		/* determine data range */
	range(x, n, &wk.lam_min, &wk.lam_max);
	range(y, n, &wk.phi_min, &wk.phi_max);
		/* set base  data range */
	wk.lam_base = *x;
	wk.phi_base = *y;
		/* compress x and shift x and y */
	cmprs(x, y, n);
		/* determine byte length */
	len = analys(x, y, n);
	wk.cntrl = (wk.cntrl & 0x1f) + ((len - 1) << 5);
	wk.count = n - 1;
	wk.location = last_loc;
	last_loc += wk.count * 2 * len;
	return (&wk);
}
	static /* reduce longitude range pi ambiguity */
reduce(x, n) long *x; int n; {
	long d;

	while (iabs(*x) > IPI)
		*x -= isign(ITWO_PI, *x);
	while (--n) {
		d = x[1] - x[0];
		if (iabs(d) > IPI)
			x[1] -= isign(ITWO_PI, d);
		++x;
	}
}
	static /* clear bits below specified precision */
clr_lsbs(z, n) long *z; int n; {
	if (base_s > 0)
		while (n--) {
			if (*z < 0)	*z = -((- *z + round_s) & mask_s);
			else		*z = (*z + round_s) & mask_s;
			++z;
		}
}
	static /* determine range of data */
range(z, n, min, max) long *z, *min, *max; int n; {
	long mn, mx;

	mn = mx = *z++;
	while (--n) {
		if (*z < mn)	mn = *z;
		else if (*z > mx) mx = *z;
		++z;
	}
	*min = mn;
	*max = mx;
}
	static long /* rounding right shift */
rshift(v, s) long v; int s; {
	register long rnd;

	if (s) {
		rnd = 1 << (s - 1);
		if (v < 0)	v = -(( -v + rnd) >> s);
		else		v = (v + rnd) >> s;
	}
	return v;
}
	static
cmprs(x, y, n) long *x, *y; int n; {
	long x0, y0, x1, y1;
	int s0, s1, ns, s;

	y0 = y[0];
	s0 = ln2_sin(y0);
/* printf("y0: %ld, s0: %d, x0: %ld\n",y0,s0,x0); */
	x0 = rshift(x[0], s0 + base_s);
	while (--n) {
		x1 = x[1];
		s = s1 = ln2_sin( y1 = y[1] );
		if ((ns = (s1 - s0)) > 0)	s = s0;
		else if (ns)			x0 <<= -ns;
		x1 = rshift(x1, base_s + s);
		*x++ = x1 - x0;
		*y++ = (y1 - y0) >> base_s;
		x0 = ns > 0 ? rshift(x1, ns) : x1;
		s0 = s1;
		y0 = y1;
	}
}
	static /* analyse reduced data */
analys(x, y, n) long *x, *y; int n; {
	int hist[PREC+1], i, j;
 
	for (i = 0; i <= PREC; i++)	hist[i] = 0;
	while (--n) {
		i = slog2(*x++);
		j = slog2(*y++);
		hist[i > j ? i : j]++;
	}
	for (i = PREC; i >= 0; --i )
		if (hist[i])	break;
	return ((i - 1) / BITS_BYTE + 1);
}
	static /* determine number of bits required by signed number */
slog2(v) long v; {
	int i;
	double frexp();

	frexp((double)(v < 0? ~v : v), &i);
	return (i + 1);
}
	/* determine ln(base 2) of sin(pi - |x|) */
# define MAX_SHIFT 16
ln2_sin(x) long x; {
	int shift = 0;

	x = IONE / (IHALF_PI + (x > 0 ? -x : x));
	while ( shift < MAX_SHIFT && (x >>= 1) ) ++shift;
	return shift;
}
