#define EXTERN

#include "inter.h"

newsim(stats_fd) FILE *stats_fd;
{

    FILE *drain_fd;
    int i;
    int cmp();

    int num;
    char *err_buf;
    char readbuf[1024];

    fgets(readbuf,1024,stats_fd);

    if (strncmp(readbuf,"watershed",strlen("watershed")) == 0)
    {
        wat_flag = 1;
        if (strncmp(readbuf,"watershed -d",strlen("watershed -d")) == 0)
            depres_flag = 1;
    }
    else if (strncmp(readbuf,"division",strlen("division")) == 0)
    {
        wat_flag = 0;
        if (strncmp(readbuf,"division -d", strlen("division -d")) == 0)
            depres_flag = 1;
    }
    else
    {
        fprintf(stderr, "Error in reading stats file -- must be identified as\n");
        fprintf(stderr, "watershed or division\n");
        exit(6);
    }

    if (!wat_flag)
    {
        fprintf(stderr,"Error - must have watershed type of statistics file\n");
        exit(-1);
    }

    err_buf="Error in reading stats file";

    if (!(fgets(readbuf,1024,stats_fd) &&
      (sscanf(readbuf,"number of segments:  %d", &divnum) == 1)))
    {
        fprintf(stderr,"unable to read number of segments\n");
        G_fatal_error(err_buf);
    }

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

    drain_fd = G_fopen_old("watershed/drain",extthin_name,extthin_mapset);
    if (!drain_fd)
    {
        fprintf(stderr,"unable to open drainage pattern file for [%s]\n",
          extthin_name);
        exit(2);
    }

    for (i=0; i<=(divnum-2); i++)
    {
        if (fscanf(drain_fd,"%d drains into %d\n",&drain_pat[i].fromnum,
          &drain_pat[i].tonum) != 2)
        {
            fprintf(stderr,"Error reading drainage pattern file [%s]\n",
              extthin_name);
            exit(2);
        }
    }
    fclose(drain_fd);

/*  sort on fromnum */

    qsort(drain_pat,divnum-1,sizeof(DRAIN_PAT),cmp);

    tape1_fd = G_fopen_new(dir,"tape1");
    if (!tape1_fd)
    {
        fprintf(stderr,"problem opening ARMSED input files\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }
    fclose(tape1_fd);

    tape2_fd = G_fopen_new(dir,"tape2");
    if (!tape2_fd)
    {
        fprintf(stderr,"problem opening ARMSED input files\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }
    fclose(tape2_fd);

    tape3_fd = G_fopen_new(dir,"tape9");
    if (!tape3_fd)
    {
        fprintf(stderr,"problem opening ARMSED input files\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }
    fclose(tape3_fd);

    tape4_fd = G_fopen_new(dir,"tape10");
    if (!tape4_fd)
    {
        fprintf(stderr,"problem opening ARMSED input files\n");
        fprintf(stderr,"directory is %s\n",dir);
        exit(2);
    }
    fclose(tape4_fd);

    get_sim();

    for (i=0; i < divnum; i++)
    {
        var_init();

/*
        if (!wat_flag)
        {
*/
        if (sscanf(fgets(readbuf,1024,stats_fd),"segment:  %d",&num) != 1)
        {
            fprintf(stderr,"error in reading segment number\n");
            G_fatal_error(err_buf);
        }

/* for right now all units must be planar, as for right now the
only way to run this interface is with output from the watershed
software package.  eventually, this interface should be run as
a separate unit, and so the code commented out below should be
reinstated  */
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

/* if (wat_flag) inserted here if decide to do "division" type files */

        if (sscanf(fgets(readbuf,1024,stats_fd),
          "chlength:  %lf",&chlength) != 1)
        {
            fprintf(stderr,"error reading channel length\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),
          "chslope:  %lf",&chslope) != 1)
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

        if (sscanf(fgets(readbuf,1024,stats_fd),
          "olslope:  %lf",&olslope[0]) != 1)
        {
            fprintf(stderr,"error getting overland slope\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),
          "flow length:  %lf",&flow[0]) != 1)
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

        if (sscanf(fgets(readbuf,1024,stats_fd),
          "olslope:  %lf",&olslope[1]) != 1)
        {
            fprintf(stderr,"error getting overland slope\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),
          "flow length:  %lf",&flow[1]) != 1)
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

/*
            else
            {

                read_stats(0,stats_fd);

                get_data(i);

                write1(i);
                if (unit_type[i] == 1) write10(i);

            }

        }
*/

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

}

static
cmp(a,b)

    DRAIN_PAT *a, *b;
{
    if (a->fromnum > b->fromnum) return 1;
    if (a->fromnum < b->fromnum) return -1;
    return 0;
}
