#include <gis.h>
#include "local_proto.h"


extern FILE *infile, *outfile;

next_point (double *U_X, double *U_Y, Site **si)
{
	static int first=1;
	static int ndim, ndec;
	static RASTER_MAP_TYPE rtype;
        static Site *s;


	if(first){
	int nstr;
	    first=0;
	    rtype = -1;
	    G_site_describe (infile, &ndim, &rtype, &nstr, &ndec);
	    s = *si = G_site_new_struct(rtype,ndim,nstr,ndec); 
	}
	if (G_site_get (infile, s) < 0){
	    return 0;
	}
        
        *U_X=s->east;
        *U_Y=s->north;

	return 1;
}
