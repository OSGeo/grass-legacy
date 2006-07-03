/****************************************************************************
 *
 * MODULE:       g.transform
 * AUTHOR(S):    Brian J. Buckley<br>
 *               Glynn Clements
 * PURPOSE:      Utility to compute transformation based upon GCPs and 
 *               output error measurements
 * COPYRIGHT:    (C) 2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/imagery.h>

extern int CRS_compute_georef_equations(struct Control_Points *,double *, double *,double *,double *,int);
extern int CRS_georef(double,double,double *,double *,double *,double *,int);

struct Max
{
	int idx;
	double val;
};

struct Stats
{
	struct Max x, y, g;
	double sum2, rms;
};

static char *name;
static int order;

static struct Control_Points points;

static int equation_stat;

static int count;
static struct Stats fwd, rev;

static void update_max(struct Max *m, int n, double k)
{
	if (k > m->val) 
	{
		m->idx = n;
		m->val = k;
	}
}

static void update_stats(struct Stats *st, int n, double dx, double dy)
{
	double d2 = dx*dx + dy*dy;
	double dg = sqrt(d2);

	st->sum2 += d2;

	update_max(&st->x, n, dx);
	update_max(&st->y, n, dy);
	update_max(&st->g, n, dg);
}

static void compute_transformation(void)
{
	static const int order_pnts[3] = {3, 6, 10};
	double E12[10], N12[10], E21[10], N21[10];
	int n;

	equation_stat = CRS_compute_georef_equations(&points, E12, N12, E21, N21, order);

	if (equation_stat == 0)
		G_fatal_error("Not Enough Points -- %d are required.", order_pnts[order - 1]);

	if (equation_stat <= 0)
		return;

	count = 0;

	for (n = 0; n < points.count; n++)
	{
		double e1, n1, e2, n2;
		double dx, dy;

		if (points.status[n] <= 0)
			continue;

		count++;

		CRS_georef(points.e2[n], points.n2[n], &e1, &n1, E21, N21, order);
		CRS_georef(points.e1[n], points.n1[n], &e2, &n2, E12, N12, order);

		/* forward */

		dx = fabs(e1 - points.e1[n]);
		dy = fabs(n1 - points.n1[n]);

		update_stats(&fwd, n, dx, dy);

		/* reverse */

		dx = fabs(e2 - points.e2[n]);
		dy = fabs(n2 - points.n2[n]);

		update_stats(&rev, n, dx, dy);
	}

	/* compute overall RMS error */

	if (count > 0)
	{
		fwd.rms = sqrt(fwd.sum2/count);
		rev.rms = sqrt(rev.sum2/count);
	}
}

static void do_max(char name, const struct Max *m)
{
	printf("%c[%d] = %.2f\n", name, m->idx, m->val);
}

static void do_stats(const char *name, const struct Stats *st)
{
	printf("%s:\n", name);
	do_max('x', &st->x);
	do_max('y', &st->y);
	do_max('g', &st->g);
	printf("RMS = %.2f\n", st->rms);
}

static void analyze(void)
{
	if (equation_stat == -1)
		G_warning("Poorly placed control points");
	else if (equation_stat == -2)
		G_fatal_error("Insufficient memory");
	else if (equation_stat < 0)
		G_fatal_error("Parameter error");
	else if (equation_stat == 0)
		G_fatal_error("No active control points");
	else
	{
		printf("Number of active points: %d\n", count);
		do_stats("Forward", &fwd);
		do_stats("Reverse", &rev);
	}
}

int main(int argc, char **argv)
{
	struct Option *grp, *val;
	struct GModule *module;

	G_gisinit(argv[0]);

	/* Get Args */
	module = G_define_module();
	module->description =
		_("Computes a coordinate transformation based on the control points");

	grp = G_define_option();
	grp->key             = "group";
	grp->type            = TYPE_STRING;
	grp->required        = YES;
	grp->gisprompt       = "old,group,group";
	grp->description     = _("Name of imagery group");

	val = G_define_option();
	val->key             = "order";
	val->type            = TYPE_INTEGER;
	val->required        = YES;
	val->options         = "1-3";
	val->description     = _("Rectification polynomial order");

	if (G_parser (argc, argv))
		exit (-1);

	name = grp->answer;
	order = atoi(val->answer);

	I_get_control_points(name, &points);

	compute_transformation();

	analyze();

	I_put_control_points(name, &points);

	return 0;
}

