#include <stdio.h>
#include <ctype.h>

#include "projects.h"

/********************* PARMS FOR PROJ **********************/ 

#define STP_FT 	0.30480060960121920243

static double STP = 1.0;
static double METERS_in = 1.0, METERS_out = 1.0;

/* typedef struct { double u, v; }		UV; */

int pj_do_proj(x,y,info_in,info_out)
  double *x,*y;
  struct pj_info *info_in,*info_out;
{
        int inverse; 
        UV  data;

        METERS_in = info_in->meters;
        METERS_out = info_out->meters;

        if (strncmp(info_in->proj,"ll",2) == 0 ) {
          if (strncmp(info_out->proj,"ll",2) == 0 ) return -1;
          else  {
            data.u = (*x) / RAD_TO_DEG;
            data.v = (*y) / RAD_TO_DEG;
            data = pj_fwd(data,info_out->pj);
            *x = data.u / METERS_out;
            *y = data.v / METERS_out;
          }
        } 
        else {
          if (strncmp(info_out->proj,"ll",2) == 0 ) {
            data.u = *x * METERS_in;
            data.v = *y * METERS_in;
            data = pj_inv(data,info_in->pj);
            *x = data.u * RAD_TO_DEG;
            *y = data.v * RAD_TO_DEG;
          }
          else {
            data.u = *x * METERS_in;
            data.v = *y * METERS_in;
            data = pj_inv(data,info_in->pj);
            data = pj_fwd(data,info_out->pj);
            *x = data.u / METERS_out;
            *y = data.v / METERS_out;
          }
       }
       return 1;
}
 
       
