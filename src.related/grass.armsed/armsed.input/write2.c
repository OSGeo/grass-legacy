#define EXTERN extern

#include "inter.h"

write2()

{

    int i;
    int count;

    count=1;

    fprintf(temp2_fd,"%-80s\n",sim_title);

    fprintf(temp2_fd,"%10.1f%10.1f\n",
      (float)DTIM, (float)FTIM);

    fprintf(temp2_fd,"%10d%10d\n",
      NPL,NWS);

    for (i=0; i<=(divnum-1); i++)
    {
        if (unit_type[i] == 1)
        {
            fprintf(temp2_fd,"%10d%10d%10d\n%10d%10d%10d\n",
              count,unit_type[i],print[i],count+1,unit_type[i],print[i]);
            count += 2;
        }
        else
            fprintf(temp2_fd,"%10d%10d%10d\n",
              count++,unit_type[i],print[i]);
    }

}
