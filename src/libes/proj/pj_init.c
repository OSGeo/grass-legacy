/* projection initialization entry and support */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_init.c,v 4.7 1992/07/14 01:28:59 gie Exp $";
#endif
#define __PJ_LIB
#include "projects.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#define SIXTH .1666666666666666667 /* 1/6 */
#define RA4 .04722222222222222222 /* 17/360 */
#define RA6 .02215608465608465608 /* 67/3024 */
#define RV4 .06944444444444444444 /* 5/72 */
#define RV6 .04243827160493827160 /* 55/1296 */
#define MAX_ARG 50
	static int
pargc;
	static char *
pargv[MAX_ARG+1];
	static char
parafield[100];
	static
PJ *PIN = 0;

static void
#ifdef __STDC__
addarg(int type, char *n, char *v)
#else
addarg(type, n, v)
    int type;
    char *n;
    char *v;
#endif
{
	paralist *t;

	(void)strcpy(parafield, n);
	(void)strcat(parafield, "=");
	(void)strcat(parafield, v ? v : (type == 'b' ? "T" : "F"));
	if (PIN) {
		if (!(t = (paralist *)malloc(sizeof(paralist) + strlen(parafield))))
			emess(2,"parameter list mem allocation failure");
		t->next = PIN->params;
		(void)strcpy(t->param, parafield);
		PIN->params = t;
	}
}

PVALUE *
pj_param(opt, def)
char *opt, *def; {
	int type, p, l;
	char *s;
	static PVALUE value;

	type = *opt++;
	/* simple linear lookup */
	l = strlen(opt);
	for (p = 0; p < pargc; ++p) {
		s = pargv[p];
		if (!strncmp(s, opt, l) && (!s[l] || s[l] == '='))
			break;
	}
	if (type != 't') {	/* not boolean */
		if (p < pargc)
			def = s + l + (s[l] == '=' ? 1 : 0);
		addarg(type, opt, def);
		switch (type) {
		case 'i':	/* integer input */
			value.i = def ? atoi(def) : 0;
			break;
		case 'd':	/* simple real input */
			value.f = def ? atof(def) : 0.;
			break;
		case 'r':	/* degrees input */
			value.f = def ? dmstor(def, 0) : 0.;
			break;
		case 's':	/* char string */
			value.s = def;
			break;
		case 'b':	/* boolean */
			if (!def)
				value.i = 1;
			else if (*def == 'F' || *def == 'f')
				value.i = 0;
			else if (*def == '\0' || *def == 'T' || *def == 't')
				value.i = 1;
			else
				emess(1,"invalid boolean condition for %s", s);
			break;
		default:
			emess(1,"invalid \"pj_param\" request"); /* fatal, abort */
		}
	} else /* return 1 if present (ie. didn't fall out of lookup loop */
		value.i = p < pargc;
	return &value;
}

static int /* initialize geographic shape parameters */
#ifdef __STDC__
ell_set(PJ *P)
#else
ell_set(P)
    PJ *P;
#endif
{
	double b;

		/* check for varying forms of eccentricity input */
	P->es = b = 0.;
	/* R takes precedence */
	if (pj_param("tR", 0)->i)
		P->a = pj_param("dR", "")->f;
	else { /* probable elliptical figure */
		if (pj_param("tes","")->i) /* eccentricity squared */
			P->es = *(double *)pj_param("des", 0);
		else if (pj_param("te","")->i) { /* eccentricity */
			P->e = pj_param("de", 0)->f;
			P->es = P->e * P->e;
		} else if (pj_param("trf","")->i) { /* recip flattening */
			P->es = pj_param("drf", 0)->f;
			if (!P->es) {
				emess(-1,"reciprocal flattening = 0");
				return 1;
			}
			P->es = 1./P->es;
			P->es = P->es * (2. - P->es);
		} else if (pj_param("tf","")->i) { /* flattening */
			P->es = pj_param("df", 0)->f;
			P->es = P->es * (2. - P->es);
		} else if (pj_param("tb","")->i) /* minor axis */
			b = pj_param("db", 0)->f;
		if (!(P->a = pj_param("da","")->f)) {
			emess(-1,"major axis, radius = 0, or not spec'ed");
			return 1;
		}
		if (b) /* P->es from minor axis input */
			P->es = 1. - (b * b) / (P->a * P->a);
		if (pj_param("bRa", "F")->i) { /* sphere--area of ellipsoid */
			P->a *= 1. - P->es * (SIXTH + P->es * (RA4 + P->es * RA6));
			P->es = 0.;
		} else if (pj_param("bRv", "F")->i) { /* sphere--volume of ellipsoid */
			P->a *= 1. - P->es * (SIXTH + P->es * (RV4 + P->es * RV6));
			P->es = 0.;
		}
	}
	/* set remainder of ellipsoid structure */
	if (P->es < 0.)
		{ emess(-1,"squared eccentricity < 0"); return 1; }
	if (P->a == 0.)
		{ emess(-1,"major axis or radius zero"); return 1; }
	P->e = sqrt(P->es);
	P->ra = 1./P->a;
	P->one_es = 1. - P->es;
	if (P->one_es == 0.)
		{ emess(-1,"effective eccentricity = 1."); return 1; }
	P->rone_es = 1./P->one_es;
	return 0;
}
PJ *
#ifdef __STDC__
pj_init(int argc, char **argv)
#else
pj_init(argc, argv)
    int argc;
    char **argv;
#endif
{
	char *s, *name;
	void *(*proj) PROTO((PJ *));
	int i;

	/* make arguments available to pj_param */
	for (pargc = 0; pargc < argc && pargc < MAX_ARG; ++pargc)
		pargv[pargc] = argv[pargc];
	if (pargc >= MAX_ARG)
		{ emess(-1,"%d arguments is too many", pargc); return 0; }
	/* find projections selection */
	if (!(name = pj_param("sproj", 0)->s))
		{ emess(-1,"projection name not specified"); return 0; }
	for (i = 0; (s = pj_list[i].id) && strcmp(name, s) ; ++i) ;
	if (!s)
		{ emess(-1,"%s unknown projection id", name); return 0; }
	proj = pj_list[i].proj;
	/* allocate projection structure */
	if (!(PIN = (*proj)(0)))
		{ emess(-1,"projection mem alloc. failed"); return 0; }
	if (!(PIN->params  = (paralist *)malloc(sizeof(paralist) +
		strlen(parafield))))
		emess(2,"parameter list mem allocation failure");
	PIN->params->next = 0;
	(void)strcpy(PIN->params->param, parafield);
	/* check is "ellps" selected */
	if (pj_param("tellps", 0)->i) {
		if (!(name = pj_param("sellps", 0)->s))
			emess(1,"invalid specification of ellps name");;
		for (i = 0; (s = pj_ellps[i].id) && strcmp(name, s) ; ++i) ;
		if (!s)
			{ emess(-1,"no %s elliptical consts found", name); return 0; }
		if (pargc + 2 >= MAX_ARG)
			{ emess(-1,"exceeded maximum arguments"); return 0; }
		pargv[pargc++] = pj_ellps[i].major;
		pargv[pargc++] = pj_ellps[i].ell;
	}
	/* set ellipsoid/sphere parameters */
	if (ell_set(PIN))
		{ free(PIN); return 0; }
	/* set PIN->geoc coordinate system */
	if  (PIN->es)
		PIN->geoc = pj_param("bgeoc", "F")->i;
	/* PIN->over-ranging flag */
	PIN->over = pj_param("bover", "F")->i;
	/* central meridian */
	PIN->lam0 = pj_param("rlon_0", "0")->f;
	PIN->phi0 = pj_param("rlat_0", "0")->f;
	/* false easting and northing */
	PIN->x0 = pj_param("dx_0", "0")->f;
	PIN->y0 = pj_param("dy_0", "0")->f;
	/* projection specific initialization */
	return (PIN = (*proj)(PIN));
}
void
#ifdef __STDC__
pj_free(PJ *P)
#else
pj_free(P)
    PJ *P;
#endif
{
	if (P) {
		paralist *t;

		for (t = P->params; t ; t = t->next)
			free(t);
		P->pfree(P);
	}
}
#define LINE_LEN 72
void
#ifdef __STDC__
pj_pr_list(PJ *P)
#else
pj_pr_list(P)
    PJ *P;
#endif
{
	paralist *t;
	int l, n = 1;

	putchar('#');
	for (t = P->params; t; t = t->next) {
		l = strlen(t->param) + 1;
		if (n + l > LINE_LEN) {
			(void)fputs("\n#", stdout);
			n = 1;
		}
		putchar(' ');
		(void)fputs(t->param, stdout);
		n += l;
	}
	putchar('\n');
}
