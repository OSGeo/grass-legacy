#include "viz.h"
#include "G3d.h"

#define	XDIMYDIM	(Headfax->xdim)*(Headfax->ydim)
#define NO_RESAMPLE

r3read_level (g3map, Headfax, data, n_lev)
    void      *g3map;
    file_info *Headfax;
    float     *data;
    int        n_lev;
{
#ifdef NO_RESAMPLE
    G3d_getBlock (g3map, 0, 0, n_lev, 
	     Headfax->xdim, Headfax->ydim, 1, (char *)data, G3D_FLOAT);
#else
/*
    G3d_getAllignedVolume (map, originNorth, originWest, originBottom, 
                                lengthNorth, lengthWest, lengthBottom, 
				nx, ny, nz, volumeBuf, type);
    G3d_getAllignedVolume (g3map, g3map->south, g3map->west, g3map->bottom,
				g3map->, g3map->, g3map->,
				g3map->, g3map->, 1,
				(char *)data, G3D_FLOAT);

*/
#endif

    return 0;
}



