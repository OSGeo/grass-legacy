#define EXTERN extern

#include "inter.h"

write10(i) int i;

{

    int size;

    fprintf(temp4_fd,"\n\n%10d\n",unit_type[i]);

    fprintf(temp4_fd,"%10.1f%10.6f%10.3f%10.4f%10.4f%10.4f%10.4f%10.2f\n",
      chlength,chslope,chKw,chporos,chSi,chSw,chYc,Chplast);

    fprintf(temp4_fd,"%10.4f%20.1f%10.6f\n",
      Mann_n,Temp,COHM);

    fprintf(temp4_fd,"%10.2f%10.2f%10.2f%10.2f%10.2f%10.1f%10.5f%10.4f\n",
      a[0],b[0],a[1],b[1],AGB,BEX,DCOEFF,Taock);

    for (size=0; size<=(num_sizes-1); size++)
    {
        fprintf(temp4_fd,"%10.4f%10.4f\n",
          sed[size].sed_size, sed[size].per_finer);
    }
}
