
#define EXTERN extern
#include "inter.h"

var_init(i) int i;
{
    int k;

    chlength = 0.;
    chslope = 0.;

    for (k=0; k<=1; k++)
    {
        olslope[k] = 0;
        flow[k] = 0;
        porosity[k] = 0.;
        Kw[k] = 0.;
        Yc[k] = 0.;
        Dof[k] = 0.;
        K[k] = 0.;
        plasticity[k] = 0;
        Tc[k] = .047;
        VC[k] = .1;
        VG[k] = .1;
        Si[k] = 0.5;
        Sw[k] = 1.0;
        Mann_n = 0.035;
        a[k] = 0.;
        b[k] = 0.;
        z[k] = 0.;
        ADW[k] = 0.;
    }

    for (k=0; k<=5; k++)
        cover[k] = 0.;

    if (!rain_flag)
    {
        NRAIN1 = 0;
        NRAIN2 = 0;
        if (i != 0)
        {
            free(rain1);
            free(rain2);
        }
    }

    if (i != 0)
    {
        for (k=0; k<2*num_sizes; k++)
            sed[k].per_finer = 0.;
    }

    ChDof = 0.;
    chlength = 0.;
    chslope = 0.;
    *unit_ident = 0;
}
