#define EXTERN extern

#include "gis.h"
#include "edit.h"

read_fp(fd,num_wtable) FILE *fd; int num_wtable;
{
    char buf[100];
    char readbuf[1024];
    int i, j;

    sprintf(buf,"Error reading firing point file -- incorrect format\n");

    for (i=0; i<num_wtable; i++)
    {

        if (fgets(readbuf,1024,fd) == NULL)
            G_fatal_error(buf);
        if (sscanf(readbuf,"%s %lf %lf %f %d",
          temp_fp[i].id, &temp_fp[i].easting,
          &temp_fp[i].northing, &temp_fp[i].gc_fact,
          &temp_fp[i].num_weap) != 5)
            G_fatal_error(buf);

        if ( (temp_fp[i].easting <= window.west) ||
            (temp_fp[i].easting >= window.east) ||
            (temp_fp[i].northing >= window.north) ||
            (temp_fp[i].northing <= window.south) )
        {
            fprintf(stderr, "Fatal error!!\n");
            fprintf(stderr, "Point coordinates outside current window for firing point %s\n",
              temp_fp[i].id);
            sleep(2);
            exit(-2);
        }

        temp_fp[i].ptr = (FP_INFO *)G_calloc(temp_fp[i].num_weap,sizeof(FP_INFO));

        for (j=0; j<temp_fp[i].num_weap; j++)
        {
            if (fgets(readbuf,1024,fd) == NULL)
                G_fatal_error(buf);

            if (sscanf(readbuf,"%d %d %d %d %d %s %s %f",
              &temp_fp[i].ptr[j].weap_code, &temp_fp[i].ptr[j].num_day,
              &temp_fp[i].ptr[j].num_night, &temp_fp[i].ptr[j].min_charge,
              &temp_fp[i].ptr[j].max_charge, temp_fp[i].ptr[j].targ_id,
              temp_fp[i].ptr[j].noise, &temp_fp[i].ptr[j].height) != 8)
                G_fatal_error(buf);
            if (strncmp(temp_fp[i].ptr[j].targ_id,"No",2) == 0)
                temp_fp[i].ptr[j].targ_id[0] = 0;

        }

    }
/*
for (i=0; i<num_wtable; i++)
{
fprintf(stderr,"info for firing point number %d\n",i);
fprintf(stderr,"%s %lf %lf %f %d\n",
          temp_fp[i].id, temp_fp[i].easting,
          temp_fp[i].northing, temp_fp[i].gc_fact,
          temp_fp[i].num_weap);
for (j=0; j<temp_fp[i].num_weap; j++)
fprintf(stderr,"%d %d %d %d %d %s %s %f\n",
              temp_fp[i].ptr[j].weap_code, temp_fp[i].ptr[j].num_day,
              temp_fp[i].ptr[j].num_night, temp_fp[i].ptr[j].min_charge,
              temp_fp[i].ptr[j].max_charge, temp_fp[i].ptr[j].targ_id,
              temp_fp[i].ptr[j].noise, temp_fp[i].ptr[j].height);
}
sleep(3);
*/
}
