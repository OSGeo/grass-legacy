#define EXTERN extern

#include "gis.h"
#include "water.h"

read_title(proj_fd) FILE *proj_fd;
{
    char buf[100];
    char stepbuf[80];
    char readbuf[1024];
    int num;
    int i;
    char *tempbuf;
    char mapset[30];

    sprintf(buf,"Error reading project file -- incorrect format\n");

    if (sscanf(fgets(readbuf,1024,proj_fd),
      "Project name:  %s in %s",proj_name, proj_mapset) != 2)
    {
        G_fatal_error(buf);
    }

/* fprintf(stderr,"project name is    %s in %s\n",proj_name, proj_mapset); */

    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);

/* fprintf(stderr,"next non-empty line is  %s\n",readbuf); */

    strcpy(stepbuf,step1);
    strcat(stepbuf,"   COMPLETE\n");
    if (strncmp(stepbuf,readbuf,strlen(stepbuf)) == 0)
        complete[1] = 1;
    else
    {
        if (strncmp(step1,readbuf,strlen(step1)) == 0)
            complete[1] = 0;
        else
            G_fatal_error(buf);
    }

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"elev:",5) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"elev:  %s in %s", elev_name, mapset) != 2)
    {
        elev_name[0] = 0;
    }
    else
    {
        elev_mapset = G_store(mapset);
    }

/* fprintf(stderr,"elev filename is... %s in %s\n",elev_name, elev_mapset); */

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"filt_elev:",10) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"filt_elev:  %s in %s", filt_name, mapset) != 2)
    {
        filt_name[0] = 0;
    }
    else
    {
        filt_mapset = G_store(mapset);
    }

/* fprintf(stderr,"filt filename is... %s in %s\n",filt_name, filt_mapset); */
    if (sscanf(fgets(readbuf,1024,proj_fd),"filt_num:  %d", &filt_num) != 1)
        G_fatal_error(buf);

/* fprintf(stderr,"filter numbers ... %d",filt_num); */
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);

    strcpy(stepbuf,step2);
    strcat(stepbuf,"   COMPLETE\n");
    if (strncmp(stepbuf,readbuf,strlen(stepbuf)) == 0)
        complete[2] = 1;
    else
    {
        if (strncmp(step2,readbuf,strlen(step2)) == 0)
            complete[2] = 0;
        else
            G_fatal_error(buf);
    }

/* fprintf(stderr,"getting step 3 line from file\n"); */
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);

/* fprintf(stderr,"line has been read as:\n%s\n",readbuf); */

    strcpy(stepbuf,step3);
    strcat(stepbuf,"   COMPLETE\n");
    if (strncmp(stepbuf,readbuf,strlen(stepbuf)) == 0)
        complete[3] = 1;
    else
    {
        if (strncmp(step3,readbuf,strlen(step3)) == 0)
            complete[3] = 0;
        else
            G_fatal_error(buf);
    }

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"accum_file:",11) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"accum_file:  %s in %s", river_name, mapset) != 2)
    {
        river_name[0] = 0;
    }
    else
    {
        river_mapset = G_store(mapset);
    }

/* fprintf(stderr,"river file %s in %s\n",river_name, river_mapset); */
    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"aspect_file:",12) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,
      "aspect_file:  %s in %s", aspect_name, mapset) != 2)
    {
        aspect_name[0] = 0;
    }
    else
    {
        aspect_mapset = G_store(mapset);
    }
/* fprintf(stderr,"aspect file %s in %s\n",aspect_name, aspect_mapset); */

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"lakes:",6) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"lakes:  %s in %s", lakes_name, mapset) != 2)
    {
        lakes_name[0] = 0;
    }
    else
    {
        lakes_mapset = G_store(mapset);
    }

    if (sscanf(fgets(readbuf,1024,proj_fd),"outlet coordinates:   %lf %lf",
      &easting, &northing) != 2)
        G_fatal_error(buf);

/* fprintf(stderr,"outlet coords:  %lf %lf\n",easting,northing); */

    if (sscanf(fgets(readbuf,1024,proj_fd),"slope thres:   %lf", &sl_thres)
      != 1)
        G_fatal_error(buf);
/* fprintf(stderr,"slope thres: %lf\n",sl_thres); */

    if (sscanf(fgets(readbuf,1024,proj_fd),"pit thres:   %d", &pit_thres)
      != 1)
        G_fatal_error(buf);

/* fprintf(stderr,"pit thres: %d\n",pit_thres); */

    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);

    strcpy(stepbuf,step4);
    strcat(stepbuf,"   COMPLETE\n");
    if (strncmp(stepbuf,readbuf,strlen(stepbuf)) == 0)
        complete[4] = 1;
    else
    {
        if (strncmp(step4,readbuf,strlen(step4)) == 0)
            complete[4] = 0;
        else
            G_fatal_error(buf);
    }

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"thin:",5) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"thin:  %s in %s", thin_name, mapset) != 2)
    {
        thin_name[0] = 0;
    }
    else
    {
        thin_mapset = G_store(mapset);
    }

/* fprintf(stderr,"thin name %s in %s\n",thin_name, thin_mapset); */
    if (sscanf(fgets(readbuf,1024,proj_fd),"thin iters:   %d", &thin_num) != 1)
        G_fatal_error(buf);

/* fprintf(stderr,"thin iters %d\n",thin_num); */

    if (sscanf(fgets(readbuf,1024,proj_fd),"accum thres:   %d",&acc_thres) != 1)
        G_fatal_error(buf);

/* fprintf(stderr,"accum thres %d\n",acc_thres); */
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);

    strcpy(stepbuf,step5);
    strcat(stepbuf,"   COMPLETE\n");
    if (strncmp(stepbuf,readbuf,strlen(stepbuf)) == 0)
        complete[5] = 1;
    else
    {
        if (strncmp(step5,readbuf,strlen(step5)) == 0)
            complete[5] = 0;
        else
            G_fatal_error(buf);
    }

/* fprintf(stderr,"step 5 line is   %s\n",readbuf); */
    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"extthin:",8) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"extthin:  %s in %s", extthin_name, mapset) != 2)
    {
        extthin_name[0] = 0;
    }
    else
    {
        extthin_mapset = G_store(mapset);
    }

/* fprintf(stderr,"extended network name %s in %s",extthin_name, extthin_mapset); */

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"clthin:",7) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"clthin:  %s in %s", clthin_name, mapset) != 2)
    {
        clthin_name[0] = 0;
    }
    else
    {
        clthin_mapset = G_store(mapset);
    }

/* fprintf(stderr,"coded network name %s in %s\n",clthin_name, clthin_mapset);
*/
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);

    strcpy(stepbuf,step6);
    strcat(stepbuf,"   COMPLETE\n");
    if (strncmp(stepbuf,readbuf,strlen(stepbuf)) == 0)
        complete[6] = 1;
    else
    {
        if (strncmp(step6,readbuf,strlen(step6)) == 0)
            complete[6] = 0;
        else
            G_fatal_error(buf);
    }

    if (fgets(readbuf,1024,proj_fd) == NULL)
        G_fatal_error(buf);
    if (strncmp(readbuf,"basin:",5) != 0)
        G_fatal_error(buf);
    if (sscanf(readbuf,"basin:  %s in %s", basin_name, mapset) != 2)
    {
        basin_name[0] = 0;
    }
    else
    {
        basin_mapset = G_store(mapset);
    }

/* fprintf(stderr,"basin name %s in %s\n",basin_name, basin_mapset); */

    return;

}
