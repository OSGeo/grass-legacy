
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "projects.h"

#include "pjinf.h"
/********************* PARMS FOR PROJ **********************/ 
#define RAD_TO_DEG     57.29577951308232

#define STP_FT 	0.30480060960121920243

static double STP = 1.0;

/* typedef struct { double u, v; }		UV; */

int G_do_proj(x,y,info_in,info_out)
  double *x,*y;
  struct pj_info *info_in,*info_out;
{
        int inverse; 
        UV  data;

        if (strncmp(info_in->proj,"ll",2) == 0 ) {
          if (strncmp(info_out->proj,"ll",2) == 0 ) return -1;
          else  {
            if (info_out->stp) STP = STP_FT;
            else  STP  = 1.0;
            data.u = (*x) / RAD_TO_DEG;
            data.v = (*y) / RAD_TO_DEG;
            data = pj_fwd(data,info_out->pj);
            *x = data.u / STP;
            *y = data.v / STP;
          }
        } 
        else {
          if (strncmp(info_out->proj,"ll",2) == 0 ) {
            if (info_in->stp) STP = STP_FT;
            else  STP  = 1.0;
            data.u = *x * STP;
            data.v = *y * STP;
            data = pj_inv(data,info_in->pj);
            *x = data.u * RAD_TO_DEG;
            *y = data.v * RAD_TO_DEG;
          }
          else {
            if (info_in->stp) STP = STP_FT;
            else  STP  = 1.0;
            data.u = *x * STP;
            data.v = *y * STP;
            data = pj_inv(data,info_in->pj);
            *x = data.u * RAD_TO_DEG;
            *y = data.v * RAD_TO_DEG;

            if (info_out->stp) STP = STP_FT;
            else  STP  = 1.0;
            data.u = *x / RAD_TO_DEG;
            data.v = *y / RAD_TO_DEG;
            data = pj_fwd(data,info_out->pj);
            *x = data.u / STP;
            *y = data.v / STP;
          }
       }
       return 1;
}
 
       
