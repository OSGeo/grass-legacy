#ifndef lint
static char *SCCSID = "@(#)geod_setup.c	USGS v.1.2";
#endif
#include "projects.h"
#include "geodesic.h"
#ifndef DEF_ELL
# define DEF_ELL "clrk66"
#endif
	static char /* default ellipsoid */
*ell_default = DEF_ELL;
	void
geod_setup(inverse) {
	char *el;

	/* check if list of ellipsoids desired */
	if (!strcmp("list",el = (char *)(*param)("sellps",""))) {
		printf("Standard ellipsoids ([+]ellps=__) Default: %s\n",
			ell_default);
		ell_list();
		exit(0);
	}
	if (*el && SetEllips(el))
		emess(1,"unknown ellipsoid name: %s",el);
	else if (!*(char *)(*param)("sa", "") && SetEllips(ell_default))
		emess(1,"SYSTEM ERROR, default ellipse %s unknown",
			ell_default);
	ELL_set();
	if (es) {
		ellipse = 1;
		onef = sqrt(one_es);
		f = 1 - onef;
		f2 = f/2;
		f4 = f/4;
		f64 = f*f/64;
	} else {
		ellipse = 0;
		onef = 1.;
		f = f2 = f4 = f64 = 0.;
	}
		/* check if line or arc mode */
	if (*(int *)param("blat_1", "")) {
		double del_S;

		phi1 = *param("rlat_1","");
		lam1 = *param("rlon_1","");
		if (*(int *)param("blat_2", "")) {
			phi2 = *param("rlat_2","");
			lam2 = *param("rlon_2","");
			geod_invrs();
			geod_prefor();
		} else if (S = *param("dS", "")) {
			al12 = *param("rA","");
			geod_prefor();
			geod_forwd();
		} else emess(1,"incomplete geodesic/arc info");
		if ((n_alpha = *(int*)(*param)("in_A","")) > 0) {
			if (!(del_alpha = fabs(*param("rdel_A",""))))
				emess(1,"del azimuth == 0");
		} else if (del_S = fabs(*param("dd_S",""))) {
			n_S = S / del_S + .5;
		} else if ((n_S = *(int*)(*param)("in_S","")) <= 0)
			emess(1,"no interval divisor selected");
	}
}
