/*******************************************************************
 * Line drawing in the current window.
 *
 * Clip window:
 *   D_set_clip_window (top, bottom ,left, right)
 *      establish clipping region for subseqent line drawing.
 *   D_set_clip_window_to_map_window ()
 *     set clipping to pixels corresponding to the current map region
 *     (default)
 *   D_set_clip_window_to_screen_window ()
 *     set clipping to full extent of the window (ie disables clipping on screen)
 *
 * Moves.
 *   D_move_abs(x,y)   move to x,y.
 *   D_move_rel(x,y)   move to +x,+y.
 *      Set current position. Position is not clipped.
 *
 * Draw line 
 *   D_cont_abs(x,y)   draw to x,y.
 *   D_cont_rel(x,y)   draw to +x,+y.
 *      Line draw from current position. New postion is not clipped.
 *      The lines drawn are clipped however.
 *      Return values indicate the nature of the clipping:
 *        0 no clipping
 *        1 part of the line is drawn
 *       -1 none of the line is drawn
 *   
 *
 */

#include <math.h>

#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/display.h>

struct rectangle
{
	double top;
	double bot;
	double left;
	double rite;
};

struct vector
{
	double x, y;
};

struct plane
{
	double x, y, k;
};

static struct vector cur;

static struct rectangle clip;

static struct plane pl_left = {-1,  0, 0};
static struct plane pl_rite = { 1,  0, 0};
static struct plane pl_top  = { 0, -1, 0};
static struct plane pl_bot  = { 0,  1, 0};

static int window_set = 0;

#define min(x,y) ((x) < (y) ? (x) : (y))
#define max(x,y) ((x) > (y) ? (x) : (y))

#define round(x) ((int) floor(0.5 + (x)))

static int *xi, *yi;
static int nalloc_i;

static double *xf, *yf;
static int nalloc_f;

static void alloc_int(int n)
{

	if (nalloc_i >= n)
		return;

	nalloc_i = n;
	xi = G_realloc(xi, nalloc_i * sizeof(int));
	yi = G_realloc(yi, nalloc_i * sizeof(int));
}

static void alloc_float(int n)
{

	if (nalloc_f >= n)
		return;

	nalloc_f = n + 10;
	xf = G_realloc(xf, nalloc_f * sizeof(double));
	yf = G_realloc(yf, nalloc_f * sizeof(double));
}

static void dealloc_float(const double **x, const double **y, int release)
{
	if (release)
	{
		G_free(*(double **) x);
		G_free(*(double **) y);
	}

	*x = xf;
	*y = yf;

	nalloc_f = 0;

	xf = NULL;
	yf = NULL;
}

static void do_convert(const double *x, const double *y, int n)
{
	int i;

	alloc_int(n);

	for (i = 0; i < n; i++)
	{
		xi[i] = round(D_u_to_d_col(x[i]));
		yi[i] = round(D_u_to_d_row(y[i]));
	}
}

static double dist_plane(double x, double y, const struct plane *p)
{
	return x * p->x + y * p->y + p->k;
}

static double interpolate(double a, double b, double ka, double kb)
{
	return (a * kb - b * ka) / (kb - ka);
}

static int clip_plane(struct vector *a, struct vector *b, const struct plane *p, int *clipped)
{
	double ka = dist_plane(a->x, a->y, p);
	double kb = dist_plane(b->x, b->y, p);
	double kab;

	/* both outside */
	if (ka >= 0 && kb >= 0)
		return 1;

	/* both inside */
	if (ka < 0 && kb < 0)
		return 0;

	*clipped = 1;

	/* a outside - swap a and b */
	if (ka >= 0)
	{
		struct vector *t;
		double kt;

		 t =  a;  a =  b;  b =  t;
		kt = ka; ka = kb; kb = kt;
	}

	kab = kb - ka;

	b->x = interpolate(a->x, b->x, ka, kb);
	b->y = interpolate(a->y, b->y, ka, kb);

	return 0;
}

static int do_clip(struct vector *a, struct vector *b)
{
	int clipped = 0;

	if (a->x < clip.left && b->x < clip.left)
		return -1;
	if (a->x > clip.rite && b->x > clip.rite)
		return -1;
	if (a->y < clip.top && b->y < clip.top)
		return -1;
	if (a->y > clip.bot && b->y > clip.bot)
		return -1;

	if (clip_plane(a, b, &pl_left, &clipped))
		return -1;
	if (clip_plane(a, b, &pl_rite, &clipped))
		return -1;
	if (clip_plane(a, b, &pl_top , &clipped))
		return -1;
	if (clip_plane(a, b, &pl_bot , &clipped))
		return -1;

	return clipped;
}

/*!
 * \brief set clipping window
 *
 * Sets the clipping window to the pixel window that corresponds
 * to the current database region. This is the default.
 *
 *  \param top
 *  \param bottom
 *  \param left
 *  \param right
 */

void D_set_clip(double t, double b, double l, double r)
{
	clip.top  = min(t, b);
	clip.bot  = max(t, b);
	clip.left = min(l, r);
	clip.rite = max(l, r);

	pl_left.k =  clip.left;
	pl_rite.k = -clip.rite;
	pl_top.k  =  clip.top ;
	pl_bot.k  = -clip.bot ;

	window_set = 1;
}

/*!
 * \brief set clipping window to map window
 *
 * Sets the clipping window to the pixel window that corresponds to the
 * current database region. This is the default.
 *
 *  \param ~
 */

void D_clip_to_map(void)
{
	D_set_clip(
		D_get_u_north(),
		D_get_u_south(),
		D_get_u_west(),
		D_get_u_east());
}

/*!
 * \brief move to pixel
 *
 * Move without drawing to
 * pixel location <b>x,y</b>, even if it falls outside the clipping window.
 *
 *  \param x
 *  \param y
 */

void D_move_clip(double x, double y)
{
	cur.x = x;
	cur.y = y;
}

/*!
 * \brief line to x,y
 *
 * Draws a line from the
 * current position to pixel location <b>x,y.</b> Any part of the line that
 * falls outside the clipping window is not drawn.
 * <b>Note.</b> The new position is <b>x,y</b>, even if it falls outside the
 * clipping window. Returns 0 if the line was contained entirely in the clipping
 * window, 1 if the line had to be clipped to draw it.
 *
 *  \param x
 *  \param y
 *  \return int
 */

int D_cont_clip(double x, double y)
{
	struct vector a = cur;
	struct vector b;
	int clipped;

	b.x = x;
	b.y = y;

	cur = b;

	if (!window_set)
		D_clip_to_map();

	clipped = do_clip(&a, &b);

	if (clipped >= 0)
	{
		int x0 = round(D_u_to_d_col(a.x));
		int y0 = round(D_u_to_d_row(a.y));
		int x1 = round(D_u_to_d_col(b.x));
		int y1 = round(D_u_to_d_row(b.y));

		R_move_abs(a.x, a.y);
		R_cont_abs(b.x, b.y);
	}

	return clipped;
}

void D_polydots_clip(const double *x, const double *y, int n)
{
	int i, j;

	alloc_float(n);

	for (i = j = 0; i < n; i++)
	{
		if (x[i] < clip.left || x[i] > clip.rite)
			continue;
		if (y[i] < clip.top  || y[i] > clip.bot )
			continue;

		xf[j] = x[i];
		yf[j] = y[i];
		j++;
	}

	do_convert(xf, yf, n);

	R_polydots_abs(xi, yi, j);
}

void D_polyline_clip(const double *x, const double *y, int n)
{
	int i;

	if (n < 2)
		return;

	D_move_clip(x[0], y[0]);

	for (i = 1; i < n; i++)
		D_cont_clip(x[i], y[i]);
}

static int clip_polygon_plane(int *pn, const double *x, const double *y, const struct plane *p)
{
	int n = *pn;
	double x0 = x[n-1];
	double y0 = y[n-1];
	double d0 = dist_plane(x0, y0, p);
	int first_in = d0 < 0;
	int i, j;

	for (i = j = 0; i < n; i++)
	{
		double x1 = x[i];
		double y1 = y[i];
		double d1 = dist_plane(x1, y1, p);
		int in0 = d0 < 0;
		int in1 = d1 < 0;

		if (in0 && in1)
		{
			alloc_float(j + 1);
			xf[j] = x[i];
			yf[j] = y[i];
			j++;
		}
		else if (!in0 && !in1)
			/* skip */ ;
		else
		{
			alloc_float(j + 1);
			xf[j] = interpolate(x0, x1, d0, d1);
			yf[j] = interpolate(y0, y1, d0, d1);
			j++;
		}

		x0 = x1;
		y0 = y1;
		d0 = d1;
	}

	*pn = j;

	return (j == 0);
}

void D_polygon_clip(const double *x, const double *y, int n)
{
	alloc_float(n + 10);

	if (clip_polygon_plane(&n, x, y, &pl_left))
		return;

	dealloc_float(&x, &y, 0);

	if (clip_polygon_plane(&n, x, y, &pl_rite))
		return;

	dealloc_float(&x, &y, 1);

	if (clip_polygon_plane(&n, x, y, &pl_top ))
		return;

	dealloc_float(&x, &y, 1);

	if (clip_polygon_plane(&n, x, y, &pl_bot ))
		return;

	dealloc_float(&x, &y, 1);

	do_convert(x, y, n);

	R_polygon_abs(xi, yi, n);
}

void D_box_clip(double x1, double y1, double x2, double y2)
{
	double t = min(clip.top , max(y1, y2));
	double b = max(clip.bot , min(y1, y2));
	double l = max(clip.left, min(x1, x2));
	double r = min(clip.rite, max(x1, x2));
	int ti = round(D_u_to_d_row(t));
	int bi = round(D_u_to_d_row(b));
	int li = round(D_u_to_d_col(l));
	int ri = round(D_u_to_d_col(r));

	R_box_abs(l, t, r, b);
}

void D_move(double x, double y)
{
	int xi = round(D_u_to_d_col(x));
	int yi = round(D_u_to_d_row(y));

	R_move_abs(xi, yi);
}

void D_cont(double x, double y)
{
	int xi = round(D_u_to_d_col(x));
	int yi = round(D_u_to_d_row(y));

	R_cont_abs(xi, yi);
}

void D_polydots(const double *x, const double *y, int n)
{
	do_convert(x, y, n);
	R_polydots_abs(xi, yi, n);
}

void D_polyline(const double *x, const double *y, int n)
{
	do_convert(x, y, n);
	R_polyline_abs(xi, yi, n);
}

void D_polygon(const double *x, const double *y, int n)
{
	do_convert(x, y, n);
	R_polygon_abs(xi, yi, n);
}

void D_box(double x1, double y1, double x2, double y2)
{
	double t = max(y1, y2);
	double b = min(y1, y2);
	double l = min(x1, x2);
	double r = max(x1, x2);
	int ti = round(D_u_to_d_row(t));
	int bi = round(D_u_to_d_row(b));
	int li = round(D_u_to_d_col(l));
	int ri = round(D_u_to_d_col(r));

	R_box_abs(l, t, r, b);
}

