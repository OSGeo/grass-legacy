#define EXTERN extern

#include "inter.h"

read_stats(value,stats_fd)

int value;
FILE *stats_fd;

{

    char err_buf[50];
    int nodata_flag;
    char readbuf[1024];

    if (fgets(readbuf,1024,stats_fd) == NULL)
    {
        fprintf(stderr,"End of file reached unexpectedly in reading stats file\n");
        exit(6);
    }

    if (strncmp(readbuf,"soils:  no data",15) == 0)
        nodata_flag = 1;
    else if (strncmp(readbuf,"soils:",6) == 0)
        nodata_flag = 0;
   else
   {
       fprintf(stderr,"error in reading soils line\n");
       exit(7);
    }

    sprintf(err_buf,"Error in reading soils data from stats file\n");

    if (!nodata_flag)
    {
        if (!(fgets(readbuf,1024,stats_fd) &&
        sscanf(readbuf,"porosity: %f", &porosity[value]) == 1))
        {
            fprintf(stderr,"error reading porosity\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"Kw: %f",
            &Kw[value]) != 1)
        {
            fprintf(stderr,"error reading Kw\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"Yc: %f",
            &Yc[value]) != 1)
        {
            fprintf(stderr,"error reading Yc\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"plasticity: %d",
            &plasticity[value]) != 1)
        {
            fprintf(stderr,"error reading plasticity\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"K: %f",
            &K[value]) != 1)
        {
            fprintf(stderr,"error reading K\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"Dof: %f",
            &Dof[value]) != 1)
        {
            fprintf(stderr,"error reading Dof\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"finer1: %f",
            &finer1[value]) != 1)
        {
            fprintf(stderr,"error reading percent finer 1\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"finer2: %f",
            &finer2[value]) != 1)
        {
            fprintf(stderr,"error reading percent finer 2\n");
            G_fatal_error(err_buf);
        }

    }

    nodata_flag = 0;
    
/*fprintf(stderr,"stats are:  porosity %f\n  Kw %f\n Yc %f\n",
  porosity[value],Kw[value],Yc[value]);
fprintf(stderr,"  plasticity %d\n  K %f\n  Dof %f\n",plasticity[value],
  K[value],Dof[value]);
fprintf(stderr,"  finer1 %f\n finer2 %f\n",finer1[value],finer2[value]);
  sleep(2); */

    if (fgets(readbuf,80,stats_fd) == NULL)
    {
        fprintf(stderr,"End of file reached unexpectedly in reading stats file\n");
        exit(6);
    }

    if (strncmp(readbuf,"cover:  no data",15) == 0)
        nodata_flag = 1;
    else if (strncmp(readbuf,"cover:",6) == 0)
        nodata_flag = 0;
   else
   {
       fprintf(stderr,"error in reading cover line\n");
       exit(7);
    }

    sprintf(err_buf,"Error in reading cover data from stats file\n");

    if (!nodata_flag)
    {

        if (sscanf(fgets(readbuf,1024,stats_fd),"percent canopy: %f",
            &cover[value*3]) != 1)
        {
            fprintf(stderr,"error reading percent canopy\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"percent ground: %f",
            &cover[value*3+1]) != 1)
        {
            fprintf(stderr,"error reading percent ground\n");
            G_fatal_error(err_buf);
        }

        if (sscanf(fgets(readbuf,1024,stats_fd),"percent impervious: %f",
            &cover[value*3+2]) != 1)
        {
            fprintf(stderr,"error reading percent impervious\n");
            G_fatal_error(err_buf);
        }
/*fprintf(stderr,"stats are:  percent canopy: %f\n percent ground %f\n",
  cover[value*3], cover[value*3+1]);
fprintf(stderr,"percent impervious %f\n",cover[value*3+2]);
  sleep(2); */

    }

}
