#include "netcdf.h"
/* The following function will read one row 
*  at a time  from the netcdf file */

int
getsize(precip_type)
nc_type precip_type;
{
	return(nctypelen(precip_type));
}
	

int
getcols(ncdfile)
int ncdfile;
{
	
	
	long size_of_hrapx;

	ncdiminq(ncdfile, (int) ncdimid(ncdfile, "hrapx"), (char *) 0, &size_of_hrapx);

	return(size_of_hrapx);
}

int 
getrows(ncdfile)
int ncdfile;
{

	long size_of_hrapy;


	ncdiminq(ncdfile, (int)ncdimid(ncdfile, "hrapy"), (char *) 0, &size_of_hrapy);

	return(size_of_hrapy);
}

