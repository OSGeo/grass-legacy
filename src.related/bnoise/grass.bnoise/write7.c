
#define EXTERN extern

#include "gis.h"
#include "edit.h"

write7(fd) FILE *fd;

{

    int i,j;
    int noise_num;
    char temp_name[21];

    for (i=0; i<num_weapons; i++)
    {

        fprintf(fd," %2d", weapons[i].code);
        for (j=0; j<11; j++)
        {
            if (weapons[i].data[j] >= 100.)
                fprintf(fd,"%7.3f",weapons[i].data[j]);
            else if (weapons[i].data[j] >= 10.)
                fprintf(fd,"%7.4f",weapons[i].data[j]);
            else
                fprintf(fd,"%7.5f",weapons[i].data[j]);
        }
        fprintf(fd,"\n");

        strcpy(temp_name,"                    ");
        strncpy(temp_name,weapons[i].name,strlen(weapons[i].name));
        temp_name[20] = 0;

        fprintf(fd,"   %20s\n",temp_name);
/*        fprintf(fd,"   %20s",weapons[i].name); */

        if (i == num_weapons-1)
            fprintf(fd,"*  ");
        else
            fprintf(fd,"   ");

        fprintf(fd,"%6.2f",weapons[i].data[11]);

        for (j=12; j<26; j++)
        {

            if (weapons[i].data[j] >= 10.)
                fprintf(fd,"%5.2f",weapons[i].data[j]);
            else if (weapons[i].data[j] >= 0)
                fprintf(fd,"%5.3f",weapons[i].data[j]);
            else if (weapons[i].data[j] > -10.)
                fprintf(fd,"%5.2f",weapons[i].data[j]);
            else
                fprintf(fd,"%5.1f",weapons[i].data[j]);
        }
        fprintf(fd,"\n");
    }

    for (i=0; i<num_targets; i++)
    {

/* 2x,a3,1x,3f6.0  */
        if (i == num_targets-1)
            fprintf(fd,"* %3s %9.1f %9.1f%6.1f\n",
              targets[i].id, targets[i].easting,
              targets[i].northing, targets[i].gc_fact);
        else
            fprintf(fd,"  %3s %9.1f %9.1f%6.1f\n",
              targets[i].id, targets[i].easting,
              targets[i].northing, targets[i].gc_fact);

    }

    for (i=0; i<num_fp; i++)
    {

        if (firing[i].num_weap == 0)
			continue;
/*  2x,a3,1x,3f6.0  */
        fprintf(fd,"  %3s %9.1f %9.1f%6.1f\n",
          firing[i].id, firing[i].easting,
          firing[i].northing, firing[i].gc_fact);

        for(j=0; j<firing[i].num_weap; j++)
        {
            if (strncmp(firing[i].ptr[j].noise,"y",1) == 0)
                noise_num = 0;
            else
                noise_num = 1;

            if (j == firing[i].num_weap - 1)
            {
                fprintf(fd,"*                 %2d%4d%4d%2d%2d%3s%1d%5.0f\n",
                  firing[i].ptr[j].weap_code, firing[i].ptr[j].num_day,
                  firing[i].ptr[j].num_night, firing[i].ptr[j].min_charge,
                  firing[i].ptr[j].max_charge, firing[i].ptr[j].targ_id,
                  noise_num, firing[i].ptr[j].height);
            }
            else
            {
                fprintf(fd,"                  %2d%4d%4d%2d%2d%3s%1d%5.0f\n",
                  firing[i].ptr[j].weap_code, firing[i].ptr[j].num_day,
                  firing[i].ptr[j].num_night, firing[i].ptr[j].min_charge,
                  firing[i].ptr[j].max_charge, firing[i].ptr[j].targ_id,
                  noise_num, firing[i].ptr[j].height);
            }
        }

    }

}
