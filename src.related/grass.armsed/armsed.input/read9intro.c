#define EXTERN extern

#include "inter.h"

read9intro()

{

    int i;
    int k;
    int chan_count;
    int sub;
    int *chan_num;
    int str_num;
    char fake[5];
    char buf[80];
    char readbuf[1024];

    chan_count = -1;
    strcpy(fake,"");

    fgets(readbuf,1024,temp3_fd);
    if (sscanf(readbuf,"%-80s\n",sim_title) != 1)
    {
        G_fatal_error(buf);
        exit(-3);
    }

    fgets(readbuf,1024,temp3_fd);
    if (sscanf(readbuf,"%10.1f%10.1f\n",(float)DTIM,(float)FTIM) != 2)
    {
        G_fatal_error(buf);
        exit(-3);
    }

    fgets(readbuf,1024,temp3_fd);
    if (sscanf(readbuf,"%10d%10d%10d%20d%10d\n",
      NPL,NWS,NCON,NCH,num_sizes) != 5)
    {
        G_fatal_error(buf);
        exit(-3);
    }

    if (NCH != NPL / 2)
    {
	fprintf(stderr,"Warning -- number of channels does not equal number\n");
	fprintf(stderr,"  of planes divided by 2\n");
    }

    chan_num = (int *)G_calloc(NCH,sizeof(int));
    drain = (DRAIN *)G_calloc(NCH,sizeof(DRAIN));

    for (i=0; i<=(NCH-1); i++)
    {
        for (k=0; k<=1; k++)
        {
            drain[i].ws_drain[k] = 0;
            drain[i].pl_drain[k] = 0;
            drain[i].str_drain[k] = 0;
        }
        drain[i].ws_drain[3] = 0;
        drain[i].str_drain[3] = 0;
    }

}
