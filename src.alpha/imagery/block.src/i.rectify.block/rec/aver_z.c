#include "global.h"
get_aver_elev (cpz, aver_z)
    struct Con_Points *cpz;
    double *aver_z;
    {
    double meanz;
    int i,n;

/*  Need 1 control points */
       if (cpz->count <= 0)
       {  
          return (-1);
       }

/* set average elevation from mean values of CONZ points */
       meanz = 0;
       n = 0;
       for (i = 0; i < cpz->count; i++)
       {   
          if (cpz->status[i] <= 0)
	     continue;

          n++;
          meanz  += *((cpz->z2)++);
          fprintf (Bugsr, "In ortho meanz = %f \n",meanz);

       }

       *aver_z = meanz / n;
       fprintf (Bugsr, "In ortho aver_z = %f \n",*aver_z);


/* reset pointers */
       for (i = 0; i < cpz->count; i++)
       {   
          if (cpz->status[i] <= 0)
	     continue;
          (cpz->z2)--;
       }
    }





