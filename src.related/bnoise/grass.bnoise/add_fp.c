#define EXTERN extern

#include "gis.h"
#include "edit.h"

add_fp()
{

    int i,j;
    int num;
    int ret_num;
    int count;
    int ok_flag;
    char answer;
    char get_answer();
    int error;
    int minus;
    int weap_count;
    char buf[80];
    int max_zone;
    int curr_weapnum;

    count = 0;
    ret_num = 1;

    if ((num = num_fp) == MAX_FP)
    {
        fprintf(stderr,"Maximum allowed number of firing points is %d\n",
          MAX_FP);
        fprintf(stderr,"  Limit has been reached\n");

        printf("\n Hit return to continue...");
        G_gets(buf);
        return;
    }

    temp_fp = (FIRPOINT *)G_calloc(10,sizeof(FIRPOINT));

    while(1)
    {

        for (i=0; i<10; i++)
        {
            temp_fp[i].id[0] = 0;
            temp_fp[i].easting = 0.;
            temp_fp[i].northing = 0.;
            temp_fp[i].gc_fact = 1.5;
            temp_fp[i].num_weap = 0;
        }

        V_clear();
        V_line(2,"    ENTER INFORMATION FOR ALL DESIRED FIRING POINTS");
        V_line(4,"                                  Ground Correction");
        V_line(5,"  ID:    Easting:     Northing:     Factor [1.5]");
    
        for (i=0; i<10; i++)
        {
            V_ques(temp_fp[i].id,'s',7+i,2,3);
            V_ques(&temp_fp[i].easting,'d',7+i,8,9);
            V_ques(&temp_fp[i].northing,'d',7+i,21,10);
            V_ques(&temp_fp[i].gc_fact,'f',7+i,39,5);
        }
    
        while (1)
        {
            ret_num = 1;
            V_intrpt_ok();
            if ( (ret_num = V_call()) == 0 )
                break;
    
            error = 0;
            count = 0;
    
            for (i=0; i<10; i++)
            {
                if (temp_fp[i].id[0])
                {
                    if ( (temp_fp[i].easting <= window.west) ||
                         (temp_fp[i].easting >= window.east) ||
                         (temp_fp[i].northing >= window.north) ||
                         (temp_fp[i].northing <= window.south) )
                    {
                        fprintf(stderr,
                        "Point coordinates outside window for firing point [%s]\n",
                          temp_fp[i].id);
                        error = 1;
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
            {
                for (i=0; i<10; i++)
                {
                    if (temp_fp[i].id[0])
                    {
                        if (num_fp == MAX_FP)
                        {
        fprintf(stderr,"Maximum allowed number of firing points is %d\n",
          MAX_FP);
        fprintf(stderr,"  Limit has been reached\n");
        fprintf(stderr,"  Cannot include firing point [%s] or those following\n ",
          temp_fp[i].id);
                            ret_num = 0;
                            break;
                        }
                        new_firing_point();
                        strcpy(firing[num_fp-1].id, temp_fp[i].id);
                        firing[num_fp-1].easting = temp_fp[i].easting;
                        firing[num_fp-1].northing = temp_fp[i].northing;
                        firing[num_fp-1].gc_fact = temp_fp[i].gc_fact;
                        count++;
                    }
                }
                break;
            }

        }

        if (ret_num == 0 || count<10)
            break;

    }

    free(temp_fp);

    for (j=num; j<num_fp; j++)
    {

        for (weap_count=0; weap_count<num_weapons; weap_count++)
            weapons[weap_count].include[0] = 0;

        V_clear();
        V_line(2,"  PLACE AN 'x' BESIDE EACH WEAPON TYPE FOR FIRING POINT ");
        V_line(4,"Include?  Code:          Weapon Name:");

        V_const(firing[j].id,'s',2,57,3);

        minus = 0;

        for (weap_count=0; weap_count<num_weapons; weap_count++)
        {
            V_line(6+(weap_count-minus)," ___");
            V_ques(weapons[weap_count].include,'s',6+(weap_count-minus),2,1);
            V_const(&weapons[weap_count].code,'i',6+(weap_count-minus),12,2);
            V_const(weapons[weap_count].name,'s',6+(weap_count-minus),19,60);

            if (((weap_count+1)/15) * 15 == (weap_count+1))
            {

                minus += 15;

                V_intrpt_ok();
                if ( !V_call() )
                {
                    fprintf(stderr,"Interrupt:  no weapons included...\n");
                    for (i=0; i<num_weapons; i++)
                        weapons[i].include[0] = 0;
                    return;
                }

                V_clear();
    
                V_line(2,"  PLACE AN 'x' BESIDE EACH WEAPON TYPE FOR FIRING POINT ");
                V_line(4,"Include?  Code:          Weapon Name:");

                V_const(firing[j].id,'s',2,57,3);

            }
        }

        if ((num_weapons/15)*15 != num_weapons)
        {
            V_intrpt_ok();
            if ( !V_call() )
            {
                fprintf(stderr,"Interrupt:  no weapons included...\n");
                for (i=0; i<num_weapons; i++)
                    weapons[i].include[0] = 0;
                return;
            }
        }

        for (weap_count=0; weap_count<num_weapons; weap_count++)
        {
            if (weapons[weap_count].include[0])
            {
                curr_weapnum = firing[j].num_weap;
                if (firing[j].num_weap == 50)
                {
                    fprintf(stderr,
                      "Cannot have more than 50 weapons at a given firing point\n");
                    fprintf(stderr,"  no more weapons will be added\n");
                    break;
                }

                max_zone = 0;
                for (i=1; i<11; i++)
                {
                    if (weapons[weap_count].data[i])
                        max_zone++;
                }

                new_fp_info(j);
                firing[j].ptr[firing[j].num_weap-1].weap_code =
                  weapons[weap_count].code;
            
                while (1)
                {
                    V_clear();
                    V_line(2,"INFORMATION FOR FIRING POINT:      (        /           )");
    
                    V_const(firing[j].id,'s',2,31,3);
                    V_const(&firing[j].easting,'d',2,36,9);
                    V_const(&firing[j].northing,'d',2,46,10);
    
                    V_line(4,"Enter information for weapon:");
                    V_const(weapons[weap_count].name,'s',4,31,45);

                    V_line(6,"  Number of Day Firings:");
                    V_line(7,"  Number of Night Firings:");
                    V_line(8,"  Minimum Charge Zone:");
                    V_line(9,"  Maximum Charge Zone:");
                    V_line(10,"  Corresponding Target ID:");
                    V_line(11,"  Noise at the Target? [y/n]");
                    V_line(12,"  Height (in feet):");

                    V_ques(&firing[j].ptr[curr_weapnum].num_day,'i',6,30,6);
                    V_ques(&firing[j].ptr[curr_weapnum].num_night,'i',7,30,6);
                    V_ques(&firing[j].ptr[curr_weapnum].min_charge,'i',8,30,2);
                    V_ques(&firing[j].ptr[curr_weapnum].max_charge,'i',9,30,2);
                    V_ques(firing[j].ptr[curr_weapnum].targ_id,'s',10,30,3);
                    V_ques(firing[j].ptr[curr_weapnum].noise,'s',11,30,1);
                    V_ques(&firing[j].ptr[curr_weapnum].height,'f',12,30,5);
    
                    V_call();

                    error = 0;

                    if ((firing[j].ptr[curr_weapnum].num_night == 0) &&
                     (firing[j].ptr[curr_weapnum].num_day == 0))
                    {
                        fprintf(stderr,
                          "Error - must have non-zero number of firings (either day or night\n");
                        error = 1;
                    }

                    if ((firing[j].ptr[curr_weapnum].min_charge <1) ||
                      (firing[j].ptr[curr_weapnum].max_charge > max_zone))
                    {
                        fprintf(stderr,
                          "Error - chosen charge zones outside allowable limits\n");
                        fprintf(stderr,"  minimum zone must be greater than or equal to 1\n");
                        fprintf(stderr,
                          "  maximum zone for this weapon must be less than or equal to %d\n",max_zone);
                        error = 1;
                    }

                    {
/* adding use of temp variables due to inability of MASSCOMP compiler
   to handle the complete equation with all levels of structures */
                        int temp_min, temp_max;
                        temp_min = firing[j].ptr[curr_weapnum].min_charge;
                        temp_max = firing[j].ptr[curr_weapnum].max_charge;
                        if (temp_min > temp_max)
                        {
                                error = 1;
                            fprintf(stderr,
                              "Error - minimum zone number must be less than or equal to maximum zone number\n");
                            fprintf(stderr,"Min is:  %d   max is:  %d\n",
                              firing[j].ptr[curr_weapnum].min_charge,
                              firing[j].ptr[curr_weapnum].max_charge);

                        }
                    }

                    if (error)
                    {
                        printf("\n Hit return to continue...");
                        G_gets(buf);
                        continue;
                    }

                    if (strncmp(firing[j].ptr[curr_weapnum].noise,"N",1) == 0)
                        sprintf(firing[j].ptr[curr_weapnum].noise,"n");
                    if (strncmp(firing[j].ptr[curr_weapnum].noise,"Y",1) == 0)
                        sprintf(firing[j].ptr[curr_weapnum].noise,"y");

                    if (firing[j].ptr[curr_weapnum].targ_id[0])
                    {
                        if ((strncmp(firing[j].ptr[curr_weapnum].noise,"n",1) != 0) &&
                            (strncmp(firing[j].ptr[curr_weapnum].noise,"y",1) != 0))
                        {
                            fprintf(stderr,
                              "Please answer yes or no to the question of noise\n");
                            error = 1;
                        }

                        ok_flag = 0;

                        for (i=0; i<num_targets; i++)
                            if (strcmp(firing[j].ptr[curr_weapnum].targ_id,targets[i].id) == 0)
                                ok_flag = 1;

                        if (!ok_flag)
                        {
                            fprintf(stderr,
                              "Error -- target id [%s] is not allowable\n", 
                              firing[j].ptr[curr_weapnum].targ_id);
                            error = 1;
                            fprintf(stdout,"Want to be shown list of targets? [y/n] ");
                            answer = get_answer();
                            if (answer == 'y')
                                show_targets();
                        }
                        else
                        {
                            if (error)
                            {
                                printf("\nHit return to continue...");
                                G_gets(buf);
                            }
                        }
                    }
                    else
                        sprintf(firing[j].ptr[curr_weapnum].noise,"n");

                    if (!error)
                        break;
                }  /* end while */
            }  /* end if weapon is included loop */
        }  /* end for weap_count loop */
    }  /* end for loop of new firing points */
    

}
