#define EXTERN extern

#include "gis.h"
#include "edit.h"

write_fp(fd) FILE *fd;
{

    int i,j;

/*
for (i=0; i<num_fp; i++)
{
fprintf(stderr,"info for firing point number %d\n",i);
fprintf(stderr,"%s %lf %lf %f %d\n",
          firing[i].id, firing[i].easting,
          firing[i].northing, firing[i].gc_fact,
          firing[i].num_weap);
for (j=0; j<firing[i].num_weap; j++)
fprintf(stderr,"%d %d %d %d %d %s %s %f\n",
              firing[i].ptr[j].weap_code, firing[i].ptr[j].num_day,
              firing[i].ptr[j].num_night, firing[i].ptr[j].min_charge,
              firing[i].ptr[j].max_charge, firing[i].ptr[j].targ_id,
              firing[i].ptr[j].noise, firing[i].ptr[j].height);
}
sleep(4);
*/
    for (i=0; i<num_fp; i++)
    {

        fprintf(fd,"%s %lf %lf %f %d\n",
          firing[i].id, firing[i].easting,
          firing[i].northing, firing[i].gc_fact,
          firing[i].num_weap);

        for(j=0; j<firing[i].num_weap; j++)
        {
            if (firing[i].ptr[j].targ_id[0])
                fprintf(fd,"%d %d %d %d %d %s %s %f\n",
                  firing[i].ptr[j].weap_code, firing[i].ptr[j].num_day,
                  firing[i].ptr[j].num_night, firing[i].ptr[j].min_charge,
                  firing[i].ptr[j].max_charge, firing[i].ptr[j].targ_id,
                  firing[i].ptr[j].noise, firing[i].ptr[j].height);
            else
                fprintf(fd,"%d %d %d %d %d No %s %f\n",
                  firing[i].ptr[j].weap_code, firing[i].ptr[j].num_day,
                  firing[i].ptr[j].num_night, firing[i].ptr[j].min_charge,
                  firing[i].ptr[j].max_charge,
                  firing[i].ptr[j].noise, firing[i].ptr[j].height);
        }

    }

}
