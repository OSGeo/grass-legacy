#define EXTERN extern
#include "stat.h"


output(value)
{

    float porosity,Kw,Yc;
    int plasticity;
    float temp_plast;
    float K,Dof;
    float finer1, finer2;
    int max,i,index;
    float percent;
    int nodata_flag;

    nodata_flag = 0;

    if (watshed[value].count == 0)
    {
        fprintf(stderr,"error -- count for renumbered division %d is zero\n",
          value);
        exit(8);
    }

    if (soils_flag)
    {

/*  if soils file has no data area in this plane, assign no data 
    percentage to soil texture that covers maximum percent of plane */

        if (watshed[value].soils[0] != 0)
        {
            if (watshed[value].soils[0]*2 >= watshed[value].count)
            {
                fprintf(stderr,"no data in soils file covers 50 percent or more");
                fprintf(stderr," of the area\n  in division number %d\n",value);
                fprintf(stderr,"  --unable to give soil characteristic estimates\n");
                nodata_flag = 1;
            }
            else
            {
                max = 0;
                for (i=1; i<=11; i++)
                {
                    if (watshed[value].soils[i] > max)
                    {
                        max = watshed[value].soils[i];
                        index = i;
                    }
                }

                watshed[value].soils[index] += watshed[value].soils[0];
            }
        }

        if (nodata_flag)
            fprintf(output_fd,"soils:  no data\n");
        else
        {
            
            fprintf(output_fd,"soils:\n");

            porosity = 0.;
            Kw = 0.;
            Yc = 0.;
            temp_plast = 0;
            K = 0.;
            Dof = 0.;
            finer1 = 0.;
            finer2 = 0.;

            for (i=1; i<=11; i++)
            {
                if (watshed[value].soils[i] == 0) continue;
                percent = (float)watshed[value].soils[i] /
                  (float)watshed[value].count;

                porosity += percent * Portable[i-1];
                Kw += percent * Kwtable[i-1];
                Yc += percent * Yctable[i-1];
                temp_plast += percent * (float)PItable[i-1];
                K += percent * Ktable[i-1];
                Dof += percent * Doftable[i-1];
                finer1 += percent * fin1table[i-1];
                finer2 += percent * fin2table[i-1];


            }

            plasticity = temp_plast;
            fprintf(output_fd,"porosity: %f\n",porosity);
            fprintf(output_fd,"Kw: %f\n",Kw);
            fprintf(output_fd,"Yc: %f\n",Yc);
            fprintf(output_fd,"plasticity: %d\n",plasticity);
            fprintf(output_fd,"K: %f\n",K);
            fprintf(output_fd,"Dof: %f\n",Dof);
            fprintf(output_fd,"finer1: %f\n",finer1);
            fprintf(output_fd,"finer2: %f\n",finer2);

        }
    }

    nodata_flag = 0;

    if (cover_flag)
    {
        if (watshed[value].cover[0] != 0)
        {
            if (watshed[value].cover[0]*2 >= watshed[value].count)
            {
                fprintf(stderr,"no data in cover file makes up 50 percent or more");
                fprintf(stderr," of the area\n  in division number %d\n",value);
                fprintf(stderr,"  --unable to give cover characteristic estimates\n");
                nodata_flag = 1;
            }
            else
            {
                max = 0;
                for (i=1; i<=3; i++)
                {
                    if (watshed[value].cover[i] > max)
                    {
                        max = watshed[value].cover[i];
                        index = i;
                    }
                }

                watshed[value].cover[index] += watshed[value].cover[0];
            }
        }
    
        if (nodata_flag)
            fprintf(output_fd,"cover:  no data\n");
        else
        {
            fprintf(output_fd,"cover:\n");

            percent = (float)watshed[value].cover[1] /
              (float)watshed[value].count;
            fprintf(output_fd,"percent canopy: %f\n",percent);

            percent = (float)watshed[value].cover[2] /
              (float)watshed[value].count;
            fprintf(output_fd,"percent ground: %f\n",percent);

            percent = (float)watshed[value].cover[3] /
              (float)watshed[value].count;
            fprintf(output_fd,"percent impervious: %f\n",percent);

        }
    }
}
