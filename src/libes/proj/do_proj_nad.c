#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "projects.h"

/********************* PARMS FOR PROJ **********************/ 

#define STP_FT 	0.30480060960121920243

static double STP = 1.0;
static double METERS_in = 1.0, METERS_out = 1.0;

/* typedef struct { double u, v; }		UV; */

int pj_do_proj_nad(x,y,info_in,info_out)
  double *x,*y;
  struct pj_info *info_in,*info_out;
{
        int inverse; 
        projUV  data;

        METERS_in = info_in->meters;
        METERS_out = info_out->meters;

	/* convert the original data to ll, in radians */
        if (strncmp(info_in->proj,"ll",2) == 0 ) 
        {
		data.u = (*x) / RAD_TO_DEG;
		data.v = (*y) / RAD_TO_DEG;
	}
        else 
	{
		data.u = *x * METERS_in;
		data.v = *y * METERS_in;
		data = pj_inv(data,info_in->pj);
	}

	/* do the datum conversion */
	data=nad_cvt(data,INVERSE_FLAG,CONVERSION_TABLE);

	/* project ll to output */
	if (strncmp(info_out->proj,"ll",2) == 0 )
	{
		data.u = (*x) * RAD_TO_DEG;
                data.v = (*y) * RAD_TO_DEG;
        }
	else
	{
		data = pj_fwd(data,info_out->pj);
		*x = data.u / METERS_out;
		*y = data.v / METERS_out;
	}
	return 1;
}
 
       
