/* erod.c (simlib), 20.nov.2002, JH */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "site.h"
#include "bitmap.h"
#include "linkm.h"

#include "waterglobs.h"

/* divergence computation from a given field */

void erod(double **hw)
{
/* hw = sigma or gamma */

  double dyp, dyn, dya, dxp, dxn, dxa;
  int k,l;
  int l1, lp, k1, kp, ln, kn;

            
        for (k = 0; k < my; k++) {
            for (l = 0; l < mx; l++) {
		
		lp = max(0,l-2);
		l1 = lp + 1;

		dxp = (v1[k][lp] * hw[k][lp] - v1[k][l1] * hw[k][l1]) / stepx;
		ln = min(mx-1,l+1);
		l1 = ln - 1;
                dxn = (v1[k][l1] * hw[k][l1] - v1[k][ln] * hw[k][ln]) / stepx;
		dxa = 0.5 * (dxp + dxn);

                kp = max(0,k-2);
                k1 = kp + 1;

                dyp = (v2[kp][l] * hw[kp][l] - v2[k1][l] * hw[k1][l]) / stepy;
                kn = min(my-1,k+1);
                k1 = kn - 1;
                dyn = (v2[k1][l] * hw[k1][l] - v2[kn][l] * hw[kn][l]) / stepy;
                dya = 0.5 * (dyp + dyn);

		er[k][l] = (dxa + dya) / deltap;

		}
	}
}
