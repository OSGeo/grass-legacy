#define EXTERN

#include "inter.h"

oldsim()
{
    fprintf(stderr,"Error -- not available\n");
    return;

/*
    int div_flag=0;
    int extthin_flag=0;
    int old_flag=0;
    char *mapset;
    char *drain_mapset;
    FILE *drain_fd;
    char answer;
    char get_answer();
    int i;
    int scan_num;
    char buf[31];
    char buf2[41];
    FILE *proj_fd;
    int cmp();

    char *stats_mapset;
    FILE *stats_fd;
    int num;
    char *err_buf;
    char readbuf[1024];
    char command[1024];


    wat_flag = 0;
    rain_flag = 0;
    sed_flag = 0;
    sed_default = 0;
    depres_flag = 0;
    NWS = 0;
    NPL = 0;


    unit_type = (int *)G_calloc(divnum,sizeof(int));
    print = (int *)G_calloc(divnum,sizeof(int));
    number = (int *)G_calloc(divnum,sizeof(int));
    drain_pat = (DRAIN_PAT *)G_calloc(divnum-1,sizeof(DRAIN_PAT));

    for (i=0; i<divnum; i++)
    {
        unit_type[i] = 0;
        print[i] = 0;
        number[i] = 0;
    }

    for (i=0; i<=(divnum-2); i++)
    {
        drain_pat[i].fromnum = 0;
        drain_pat[i].tonum = 0;
    }

    num_sizes = 0;

    tape1_fd = G_fopen_old(dir,"tape1");
    if (!tape1_fd)
    {
        fprintf(stderr,"problem opening tape1 ARMSED input file\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }

    tape2_fd = G_fopen_old(dir,"tape2");
    if (!tape2_fd)
    {
        fprintf(stderr,"problem opening tape2 ARMSED input file\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }

    tape3_fd = G_fopen_old(dir,"tape9");
    if (!tape3_fd)
    {
        fprintf(stderr,"problem opening tape9 ARMSED input file\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }

    tape4_fd = G_fopen_old(dir,"tape10");
    if (!tape4_fd)
    {
        fprintf(stderr,"problem opening tape10 ARMSED input file\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }

    read9intro();

    get_sim();

    for (i=0; i < divnum; i++)
    {
        var_init();

        if (div_flag)
        {
            if (sscanf(fgets(readbuf,1024,stats_fd),"segment:  %d",&num) != 1)
            {
                fprintf(stderr,"error in reading segment number\n");
                G_fatal_error(err_buf);
            }

            if (num != i)
            {
                fprintf(stderr,"Error in interface:  segment number incorrect\n");
                exit(6);
            }

/* for right now, only planar type units are allowed, as all input is
coming from files created using the watershed software package.  Thus,
some code is (for now) being commented out below.  */

/*
            fgets(readbuf,1024,stats_fd);
            if (strncmp(readbuf,"type:  watershed unit",
              strlen("type:  watershed unit")) == 0)
            {
                unit_type[i] = 2;
                NWS += 1;
                number[i] = NWS;
            }
            else if (strncmp(readbuf,"type:  planar unit",
              strlen("type:  planar unit")) == 0)
            {
*/
                unit_type[i] = 1;
                NPL += 2;
                number[i] = NPL-1;
/*
            }
            else
            {
                fprintf(stderr,"Error in reading stats file:  type of");
                fprintf(stderr," unit must be either watershed or planar\n");
            }
*/

            if (wat_flag)
            {
    if (sscanf(fgets(readbuf,1024,stats_fd),"chlength:  %lf",&chlength) != 1)
                {
                    fprintf(stderr,"error reading channel length\n");
                    G_fatal_error(err_buf);
                }

    if (sscanf(fgets(readbuf,1024,stats_fd),"chslope:  %lf",&chslope) != 1)
                {
                    fprintf(stderr,"error reading channel slope\n");
                    G_fatal_error(err_buf);
                }


                if (fgets(readbuf,1024,stats_fd) == NULL)
                {
                    fprintf(stderr,"error getting line for left plane\n");
                    G_fatal_error(err_buf);
                }

                if (strncmp(readbuf,"left plane",strlen("left plane")) != 0)
                {
                    fprintf(stderr,"error comparing with phrase left plane\n");
                    G_fatal_error(err_buf);
                }

    if (sscanf(fgets(readbuf,1024,stats_fd),"olslope:  %lf",&olslope[0]) != 1)
                {
                    fprintf(stderr,"error getting overland slope\n");
                    G_fatal_error(err_buf);
                }


    if (sscanf(fgets(readbuf,1024,stats_fd),"flow length:  %lf",&flow[0]) != 1)
                {
                    fprintf(stderr,"error getting overland flow length\n");
                    G_fatal_error(err_buf);
                }

                if (depres_flag)
                {
                    if (sscanf(fgets(readbuf,1024,stats_fd),"depress:  %lf",
                      &depres[0]) != 1)
                    {
                        fprintf(stderr,"error getting fraction of depression\n");
                        G_fatal_error(err_buf);
                    }
                }

                read_stats(0,stats_fd);

                if (fgets(readbuf,1024,stats_fd) == NULL)
                {
                    fprintf(stderr,"error getting line for right plane\n");
                    G_fatal_error(err_buf);
                }

                if (strncmp(readbuf,"right plane",strlen("right plane")) != 0)
                {
                    fprintf(stderr,"error comparing with phrase right plane\n");
                    G_fatal_error(err_buf);
                }

    if (sscanf(fgets(readbuf,1024,stats_fd),"olslope:  %lf",&olslope[1]) != 1)
                {
                    fprintf(stderr,"error getting overland slope\n");
                    G_fatal_error(err_buf);
                }

    if (sscanf(fgets(readbuf,1024,stats_fd),"flow length:  %lf",&flow[1]) != 1)
                {
                    fprintf(stderr,"error getting overland flow length\n");
                    G_fatal_error(err_buf);
                }

                if (depres_flag)
                {
                    if (sscanf(fgets(readbuf,1024,stats_fd),"depress:  %lf",
                      &depres[1]) != 1)
                    {
                        fprintf(stderr,"error getting fraction of depression\n");
                        G_fatal_error(err_buf);
                    }
                }

                read_stats(1,stats_fd);

                get_data(i);

                write1(i);
                if (unit_type[i] == 1) write10(i);

            }

            else
            {

                read_stats(0,stats_fd);

                get_data(i);

                write1(i);
                if (unit_type[i] == 1) write10(i);

            }

        }
        else

            get_data(i);

    }

    write2();
    write9();

    fclose(temp1_fd);
    fclose(temp2_fd);
    fclose(temp3_fd);
    fclose(temp4_fd);

    transfer_data(temp1_name,tape1_name);
    transfer_data(temp2_name,tape2_name);
    transfer_data(temp3_name,tape3_name);
    transfer_data(temp4_name,tape4_name);

    fprintf(stdout,"Do you want to run ARMSED with these inputs? [y/n] ");
    answer = get_answer();

    if (answer == 'y')
    {
        sprintf(command,"cd %s; armsed1; armsed2; armsed3", path);
        system(command);
    }

*/
}
