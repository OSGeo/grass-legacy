#define EXTERN extern

#include "inter.h"

write9()

{

    int i;
    int k;
    int chan_count;
    int sub;
    int str_num;
    char fake[5];

    chan_count = -1;
    strcpy(fake,"");

    fprintf(temp3_fd,"%-80s\n",sim_title);

    fprintf(temp3_fd,"%10.1f%10.1f\n",
      (float)DTIM, (float)FTIM);

    NCH = NPL / 2;

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

    fprintf(temp3_fd,"%10d%10d%10d%20d%10d\n",
      NPL,NWS,NCON,NCH,num_sizes);

    for (i=0; i<=(divnum-1); i++)
    {
        if (unit_type[i] == 1)
        {
            chan_count++;
            chan_num[chan_count] = i;

            for (k=0; k<=(divnum-2); k++)
            {
                if (drain_pat[k].tonum == (i+1))
                {
                    if (unit_type[(drain_pat[k].fromnum - 1)] == 1)
                    {
                        for(str_num=0; str_num<chan_count; str_num++)
                        {
                            if (chan_num[str_num] == (drain_pat[k].fromnum - 1))
                            {
                                for (sub=0; sub<=2; sub++)
                                {
                                    if (drain[chan_count].str_drain[sub] == 0)
                                    {
                                        drain[chan_count].str_drain[sub] = str_num + 1;
                                        break;
                                    }
                                }
                                if ((sub == 2) && (drain[chan_count].str_drain[2] != (str_num + 1)))
                                {
                                    fprintf(stderr,"Error -- more than 3 channels draining into one\n");
                                    exit(8);
                                }
                            }
                        }
                    }
                    else if (unit_type[(drain_pat[k].fromnum - 1)] == 2)
                    {
                        for (sub=0; sub<=2; sub++)
                        {
                            if (drain[chan_count].ws_drain[sub] == 0)
                            {
                                drain[chan_count].ws_drain[sub] = number[(drain_pat[k].fromnum - 1)];
                                break;
                            }
                        }
                        if ((sub == 2) && (drain[chan_count].ws_drain[sub] != number[(drain_pat[k].fromnum-1)]))
                        {
                            fprintf(stderr,"Error -- more than 3 watershed units draining into one channel\n");
                            exit(8);
                        }
                    }
                }
            }

            for (sub=0; sub<=1; sub++)
            {
                if (drain[chan_count].pl_drain[sub] != 0)
                {
                    fprintf(stderr,"Error -- planes draining to channel assigned incorrectly\n");
                    fprintf(stderr,"Channel number is %d and sub is currently %d\n", chan_count, sub);
                    exit(8);
                }
                if (sub == 0)
                {
                    drain[chan_count].pl_drain[sub] = number[i];
                }
                else
                {
                    drain[chan_count].pl_drain[sub] = number[i] + 1;
                }
            }
        }
    }

    for (i=1; i<=NCH; i++)
    {
        fprintf(temp3_fd,"%5d",i);

        for (k=0; k<=2; k++)
        {
            if (drain[i-1].ws_drain[k] != 0)
                fprintf(temp3_fd,"%5d",drain[i-1].ws_drain[k]);
            else
                fprintf(temp3_fd,"%5s",fake);
        }
        for (k=0; k<=1; k++)
        {
            if (drain[i-1].pl_drain[k] == 0)
                fprintf(stderr,"Error - plane number missing for channel %d\n",(i-1));
            else
                fprintf(temp3_fd,"%5d",drain[i-1].pl_drain[k]);
        }

        fprintf(temp3_fd,"%5s",fake);

        for (k=0; k<=2; k++)
        {
            if (drain[i-1].str_drain[k] != 0)
                fprintf(temp3_fd,"%5d",drain[i-1].str_drain[k]);
            else
                fprintf(temp3_fd,"%5s",fake);
        }

        fprintf(temp3_fd,"\n");

    }

}
