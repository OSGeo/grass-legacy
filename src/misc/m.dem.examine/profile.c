#include "usgs.h"

get_profile()
{
	float junk;
	buffer += get_int(&bas_n);
	if(filestat != blocksize)
		return(0);
	buffer += get_int(&bas_e);
	buffer += get_int(&rows);
	buffer += get_int(&cols);
	buffer += get_dfloat(&P_col);
	buffer += get_dfloat(&P_row);
	buffer += get_dfloat(&bas_elev);

	buffer += get_dfloat(&junk);
	buffer += get_dfloat(&junk);

	return(1);
}
