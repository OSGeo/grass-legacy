#define EXTERN extern

#include "inter.h"

write1(i) int i;

{

    float plast;
    int k;
    int rain_count;
    int size;

    if (unit_type[i] == 1)
    {
        for (k=0; k<=1; k++)
        {
            fprintf(temp1_fd,"\n\n%-12s%d\n",unit_ident,unit_type[i]);

            plast = (float)plasticity[k];

            fprintf(temp1_fd,"%10.3f%10.4f%10.4f%10.4f%10.4f%10.2f%10.6f\n\n",
              Kw[k],porosity[k],Si[k],Sw[k],Yc[k],plast,COHM);

            fprintf(temp1_fd,"%10.1f%10.4f%10.1f%10.4f%10.1f\n\n",
              cover[(k*3)],VC[k],cover[1+(k*3)],VG[k],cover[2+(k*3)]);

            fprintf(temp1_fd,"%10.6lf%10.1lf%10.2lf\n\n",
              olslope[k],flow[k],depres[k]);

            fprintf(temp1_fd,"%10.6lf%10.1lf\n",
              chslope,chlength);

            if (k == 0)
                fprintf(temp1_fd,
	  	  "%10d%10.2f%10.4f%20.3lf%10.3lf%10.1lf%10.6f\n",
                  NRAIN1,Temp,Mann_n,a[0],b[0],ADW[k],COHM);
            else
            {
                if (rain_flag)
                    fprintf(temp1_fd,
		      "%10d%10.2f%10.4f%20.3f%10.3f%10.1f%10.6f\n",
                      NRAIN1,Temp,Mann_n,a[0],b[0],ADW[k],COHM);
                else
                    fprintf(temp1_fd,
		      "%10d%10.2f%10.4f%20.3f%10.3f%10.1f%10.6f\n",
                      NRAIN2,Temp,Mann_n,a[0],b[0],ADW[k],COHM);
            }

            if (k==0)
            {
                for (rain_count=0; rain_count<NRAIN1; rain_count++)
                {
                    fprintf(temp1_fd,"%10.3f%10.1f\n", 
                      rain1[rain_count].intensity, 
                      (float)rain1[rain_count].end_time);
                }
            }
            else
            {
                if (rain_flag)
                {
                    for (rain_count=0; rain_count<NRAIN1; rain_count++)
                    {
                        fprintf(temp1_fd,"%10.3f%10.1f\n", 
                          rain1[rain_count].intensity, 
                          (float)rain1[rain_count].end_time);
                    }
                }
                else
                {
                    for (rain_count=0; rain_count<NRAIN2; rain_count++)
                    {
                        fprintf(temp1_fd,"%10.3f%10.1f\n",
                          rain2[rain_count].intensity, 
                          (float)rain2[rain_count].end_time);
                    }
                }
            }

            fprintf(temp1_fd,"%10.5f%10.1f%10.5f%10.5f%10.4f%10.2f%10d\n",
              DCOEFF,DPOW,Dof[k],ChDof,Taock,Chplast,num_sizes);

            if (k==0)
            {
                for (size=0; size<num_sizes; size++)
                {
                    fprintf(temp1_fd,"%10.4f%10.4f\n",
                      sed[size].sed_size, sed[size].per_finer);
                }
            }
            else
            {
                for (size=num_sizes; size<2*num_sizes; size++)
                {
                    fprintf(temp1_fd,"%10.4f%10.4f\n",
                      sed[size].sed_size, sed[size].per_finer);
                }
            }
        }
    }
    else
    {

        fprintf(temp1_fd,"\n\n%-12s%d\n",unit_ident,unit_type[i]);

        plast = (float)plasticity[0];

        fprintf(temp1_fd,"%10.3f%10.4f%10.4f%10.4f%10.4f%10.2f%10.6f\n",
          Kw[0],porosity[0],Si[0],Sw[0],Yc[0],plast,COHM);

        plast = (float)plasticity[1];

        fprintf(temp1_fd,"%10.3f%10.4f%10.4f%10.4f%10.4f%10.2f%10.6f\n",
          Kw[1],porosity[1],Si[1],Sw[1],Yc[1],plast,COHM);

        fprintf(temp1_fd,"%10.1f%10.4f%10.1f%10.4f%10.1f\n",
          cover[0],VC[0],cover[1],VG[0],cover[2]);

        fprintf(temp1_fd,"%10.1f%10.4f%10.1f%10.4f%10.1f\n",
          cover[3],VC[1],cover[4],VG[1],cover[5]);

        fprintf(temp1_fd,"%10.6lf%10.1lf%10.2lf\n",
          olslope[0],flow[0],depres[0]);

        fprintf(temp1_fd,"%10.6lf%10.1lf%10.2lf\n",
          olslope[1],flow[1],depres[1]);

        fprintf(temp1_fd,"%10.6lf%10.1lf\n",
          chslope,chlength);

        fprintf(temp1_fd,"%10d%10.2f%10.4f%20.3lf%10.3lf%10.1lf%10.6f\n",
          NRAIN1,Temp,Mann_n,a[0],b[0],ADW[0],COHM);

        for (rain_count=0; rain_count<NRAIN1; rain_count++)
        {
            fprintf(temp1_fd,"%10.3f%10.1f\n", 
              rain1[rain_count].intensity,(float)rain1[rain_count].end_time);
        }

        fprintf(temp1_fd,"%10.5f%10.1f%10.5f%10.5f%10.4f%10.2f%10d\n",
          DCOEFF,DPOW,Dof[0],ChDof,Taock,Chplast,num_sizes);

        for (size=0; size<=(num_sizes-1); size++)
        {
            fprintf(temp1_fd,"%10.4f%10.4f\n",
              sed[size].sed_size, sed[size].per_finer);
        }
    }
}
