#ifndef lint
static char *SCCSID = "@(#)ProjSetup.c	USGS v.3.6";
#endif
/* Cartographic Projection System
** Basic cartographic setup procedures
*/
# include "projects.h"
#ifndef DEF_ELL
# define DEF_ELL "clrk66"
#endif
	static char /* default ellipsoid */
*ell_default = DEF_ELL;
	static int
overrange = 0,	/* allow longitude over range */
geocentric = 0; /* geocentric flag */
	static UV
(*project)();
	extern UV
(*SetProj())();
# define EPS 1.0e-12
	static UV /* forward projection entry */
forward(uv) UV uv; {
	double t;

	/* check for latitude or longitude overange */
	if ((t = fabs(uv.v)-HALFPI) > EPS) {
		uv.u = HUGE;
		return uv;
	}
	if (fabs(t) <= EPS)
		uv.v = uv.v < 0. ? -HALFPI : HALFPI;
	else if (geocentric)
		uv.v = atan(rone_es * tan(uv.v));
	uv.u -= lam0;	/* compute del lam */
	if (!overrange)
		uv.u = adjlon(uv.u); /* adjust del longitude */
	uv = (*project)(uv); /* project */
	if (uv.u != HUGE) {
		uv.u = a * uv.u + x0;
		uv.v = a * uv.v + y0;
	}
	return uv;
}
	static UV /* inverse projection entry */
inverse(uv) UV uv; {
	/* can't do preliminary checking as with forward */
	uv.u = (uv.u - x0) * ra; /* descale and de-offset */
	uv.v = (uv.v - y0) * ra;
	uv = (*project)(uv); /* inverse project */
	if (uv.u != HUGE) {
		uv.u += lam0; /* reduce from del lam */
		if (!overrange)
			uv.u = adjlon(uv.u); /* adjust longitude to CM */
		if (geocentric && fabs(fabs(uv.v)-HALFPI) > EPS)
			uv.v = atan(one_es * tan(uv.v));
	}
	return uv;
}
	UV
(*ProjSetup(inv))() {
	char *el, *proj;
	void ELL_set();
	
	/* check if list of ellipsoids desired */
	if (!strcmp("list",el = (char *)param("sellps",""))) {
		printf("Standard ellipsoids ([+]ellps=__) Default: %s\n",
			ell_default);
		ell_list();
		exit(0);
	}
	/* check if list of projections desired */
	if (!strcmp("list",proj = (char *)param("sproj",""))) {
		printf("list of projections ([+]proj=__)\n\n");
		proj_list();
		exit(0);
	}
	if (! *proj) /* check if any projection selected */
		emess(1,"projection not selected");
	/* if no el and +a and presumably no elliptical param, the
	** set default ellipse */
	if (*el && SetEllips(el))
		emess(1,"unknown ellipsoid name: %s",el);
	else if (!*(char *)param("sa", "") && SetEllips(ell_default))
		emess(1,"SYSTEM ERROR, default ellipse %s unknown",
			ell_default);
	ELL_set();
	/* set geocentric coordinate system */
	if  (es)
		geocentric = *(int *)param("bgeoc","");
	overrange = *(int *)param("bover","");
	if (!(project = SetProj(proj, inv)))
		emess(1,"unknown or for/inv mode unavail for projection: %s",
			proj);
	if (inv)
		return(inverse);
	else
		return(forward);
}
