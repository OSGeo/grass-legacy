#define EXTERN extern

#include "gis.h"
#include "edit.h"

add_target()
{

    int i;
    int error;
    int count;
    int ret_num;
    int end_num;
    char buf[80];

    if (num_targets == MAX_TARG)
    {
        fprintf(stderr,"Maximum allowed number of target points is %d\n",
          MAX_TARG);
        fprintf(stderr,"  Limit has been reached\n");

        printf("\n Hit return to continue...");
        G_gets(buf);
        return;
    }

    while(1)
    {

        if ( (MAX_TARG-num_targets)<14 )
            end_num = MAX_TARG - num_targets;
        else
            end_num = 14;

        for (i=0; i<14; i++)
        {
            temp_targ[i].id[0] = 0;
            temp_targ[i].easting = 0.;
            temp_targ[i].northing = 0.;
            temp_targ[i].gc_fact = 1.5;
        }

        V_clear();
        V_line(2,"    ENTER INFORMATION FOR ALL DESIRED TARGET POINTS");
        V_line(4,"                                  Ground Correction");
        V_line(5,"  ID:    Easting:     Northing:     Factor [1.5]");
    
        for (i=0; i<end_num; i++)
        {
            V_ques(temp_targ[i].id,'s',7+i,2,3);
            V_ques(&temp_targ[i].easting,'d',7+i,8,9);
            V_ques(&temp_targ[i].northing,'d',7+i,21,10);
            V_ques(&temp_targ[i].gc_fact,'f',7+i,39,5);
        }
    
        while (1)
        {
            ret_num = 1;
            V_intrpt_ok();
            if ( (ret_num = V_call()) == 0 )
                break;
    
            error = 0;
    
            for (i=0; i<end_num; i++)
            {
                if (temp_targ[i].id[0])
                {

                    if ( (temp_targ[i].easting <= window.west) ||
                         (temp_targ[i].easting >= window.east) ||
                         (temp_targ[i].northing >= window.north) ||
                         (temp_targ[i].northing <= window.south) )
                    {
                        error = 1;
                        fprintf(stderr,
                          "Point coordinates outside window for target point [%s]\n",
                          temp_targ[i].id);
                    }
                }
            }

            if (error)
            {
                fprintf(stderr,"Current window:\n");
                fprintf(stderr,"   east:   %lf\n   west:   %lf\n",
                  window.east, window.west);
                fprintf(stderr,"   north:  %lf\n   south:  %lf\n",
                  window.north, window.south);

                printf("\n Hit return to continue...");
                G_gets(buf);
                continue;
            }
            else
                break;
        }

        if (ret_num == 0)
            break;

        count = 0;
    
        for (i=0; i<end_num; i++)
        {
            if (temp_targ[i].id[0])
            {
                count++;

                strcpy(targets[num_targets].id,temp_targ[i].id);
                targets[num_targets].easting = temp_targ[i].easting;
                targets[num_targets].northing = temp_targ[i].northing;
                targets[num_targets].gc_fact = temp_targ[i].gc_fact;
                num_targets++;
            }
        }

        if (count < end_num)
            break;

        if (num_targets == MAX_TARG)
        {
            fprintf(stderr,"Maximum allowed number of target points is %d\n",
              MAX_TARG);
            fprintf(stderr,"  Limit has been reached\n");
            break;
        }
    }

}
