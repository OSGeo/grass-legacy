#include "vizual.h"
#include "G3d.h"

#define	XDIMYDIM	(Headfax->xdim)*(Headfax->ydim)

r3read_level (g3map, Headfax, data, n_lev)
    void      *g3map;
    file_info *Headfax;
    float     *data;
    int        n_lev;
{

    G3d_getBlock (g3map, 0, 0, n_lev, 
	     Headfax->xdim, Headfax->ydim, 1, (char *)data, G3D_FLOAT);

    return 0;
}



