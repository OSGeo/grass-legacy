#define EXTERN extern

#include "gis.h"
#include "water.h"

write_title(proj_fd) FILE *proj_fd;
{
    char buf[100];
    int i;
    char printbuf[100];

    fprintf(proj_fd,"Project name:  %s in %s\n",proj_name, proj_mapset);

    fprintf(proj_fd,"\n%s",step1);
    if (complete[1] == 1)
        fprintf(proj_fd,"   COMPLETE\n");
    else
        fprintf(proj_fd,"\n");
    
    fprintf(proj_fd,"elev:  ");
    if (elev_name[0])
    {
        if ((!complete[1]) || (!elev_mapset))
            elev_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", elev_name, elev_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"filt_elev:  ");
    if (filt_name[0])
    {
        if ((!complete[1]) || (!filt_mapset))
            filt_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", filt_name, filt_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"filt_num:   ");
    fprintf(proj_fd,"%d\n",filt_num);

    fprintf(proj_fd,"\n%s",step2);
    if (complete[2] == 1)
        fprintf(proj_fd,"   COMPLETE\n");
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"\n%s",step3);
    if (complete[3] == 1)
        fprintf(proj_fd,"   COMPLETE\n");
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"accum_file:   ");
    if (river_name[0])
    {
        if ((!complete[3]) || (!river_mapset))
            river_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", river_name, river_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"aspect_file:   ");
    if (aspect_name[0])
    {
        if ((!complete[3]) || (!aspect_mapset))
            aspect_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", aspect_name, aspect_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"lakes:   ");
    if (lakes_name[0])
    {
        if ((!complete[3]) || (!lakes_mapset))
            lakes_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", lakes_name, lakes_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"outlet coordinates:   ");
    fprintf(proj_fd,"%lf %lf\n",easting,northing);

    fprintf(proj_fd,"slope thres:   ");
    fprintf(proj_fd,"%lf\n",sl_thres);

    fprintf(proj_fd,"pit thres:   ");
    fprintf(proj_fd, "%d\n", pit_thres);

    fprintf(proj_fd,"\n%s",step4);
    if (complete[4] == 1)
        fprintf(proj_fd,"   COMPLETE\n");
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"thin:   ");
    if (thin_name[0])
    {
        if ((!complete[4]) || (!thin_mapset))
            thin_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", thin_name, thin_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"thin iters:   ");
    fprintf(proj_fd,"%d\n",thin_num);

    fprintf(proj_fd,"accum thres:   ");
    fprintf(proj_fd,"%d\n",acc_thres);

    fprintf(proj_fd,"\n%s",step5);
    if (complete[5] == 1)
        fprintf(proj_fd,"   COMPLETE\n");
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"extthin:   ");
    if (extthin_name[0])
    {
        if ((!complete[5]) || (!extthin_mapset))
            extthin_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", extthin_name, extthin_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"clthin:   ");
    if (clthin_name[0])
    {
        if ((!complete[5]) || (!clthin_mapset))
            clthin_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", clthin_name, clthin_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"\n%s",step6);
    if (complete[6] == 1)
        fprintf(proj_fd,"   COMPLETE\n");
    else
        fprintf(proj_fd,"\n");

    fprintf(proj_fd,"basin:   ");
    if (basin_name[0])
    {
        if ((!complete[6]) || (!basin_mapset))
            basin_mapset = G_mapset();
        fprintf(proj_fd,"%s in %s\n", basin_name, basin_mapset);
    }
    else
        fprintf(proj_fd,"\n");

    return;

}
