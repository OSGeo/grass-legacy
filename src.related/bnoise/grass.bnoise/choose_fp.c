#define EXTERN extern

#include "gis.h"
#include "edit.h"

choose_fp(num_wtable, add_flag) int num_wtable; int add_flag;
{

    int i,j;
    int error;
    int count;
    int minus;
    int check;
    char buf[80];


    error = 0;

    for (i=0; i<num_wtable; i++)
        temp_fp[i].include[0] = 0;

    V_clear();
    if (add_flag)
    {
        V_line(2,"PLACE AN 'x' BESIDE EACH FIRING POINT YOU WISH TO INCLUDE");
        V_line(4,"              Firing");
        V_line(5,"Include?     Point ID:     Easting:     Northing:");
    }
    else
    {
        V_line(2,"PLACE AN 'x' BESIDE EACH FIRING POINT YOU WISH TO DELETE");
        V_line(4,"              Firing");
        V_line(5,"Delete?      Point ID:     Easting:     Northing:");
    }

    minus = 0;

    for (count=0; count<num_wtable; count++)
    {
        V_line(6+(count-minus)," ___");
        V_ques(temp_fp[count].include,'s',6+(count-minus),2,1);
        V_const(temp_fp[count].id,'s',6+(count-minus),16,3);
        V_const(&temp_fp[count].easting,'d',6+(count-minus),26,11);
        V_const(&temp_fp[count].northing,'d',6+(count-minus),39,11);

        if (((count+1)/15) * 15 == (count+1))
        {

            minus += 15;

            if (add_flag)
                V_intrpt_ok();
            if ( !V_call() )
                return;

            V_clear();
    
            if (add_flag)
            {
                V_line(2,"PLACE AN 'x' BESIDE EACH FIRING POINT YOU WISH TO INCLUDE");
                V_line(4,"              Firing");
                V_line(5,"Include?     Point ID:     Easting:     Northing:");
            }
            else
            {
                V_line(2,"PLACE AN 'x' BESIDE EACH FIRING POINT YOU WISH TO DELETE");
                V_line(4,"              Firing");
                V_line(5,"Delete?      Point ID:     Easting:     Northing:");
            }

        }
    }

    if ((num_wtable/15)*15 != num_wtable)
    {
        if (add_flag)
            V_intrpt_ok();
        if ( !V_call() )
            return;
    }

    if (add_flag)
    {
        for (i=0; i<num_wtable; i++)
        {
            if ( temp_fp[i].include[0] )
            {
                if (num_fp > 0)
                {
                    for (count=0; count<num_fp; count++)
                    {
                        if (strcmp(temp_fp[i].id,firing[count].id) == 0)
                        {
                            temp_fp[i].include[0] = 0;
                            fprintf(stderr,
                              "Firing point id [%s] already used, cannot be reused\n",
                              temp_fp[i].id);
                            error = 1;
                            break;
                        }
                    }
                    if (error)
                        continue;
                }
    
/*  for each weapon at firing point, check existence of weapon in current
    weapons list  */
                for (j=0; j<temp_fp[i].num_weap; j++)
                {
                    check = 0;
                    for (count=0; count<num_weapons; count++)
                    {
                        if (temp_fp[i].ptr[j].weap_code == weapons[count].code)
                            check = 1;
                    }
                    if (!check)
                    {
                        temp_fp[i].include[0] = 0;
                        fprintf(stderr,
                        "Weapon code [%d] is not included in the current list\n",
                          temp_fp[i].ptr[j].weap_code);
                        fprintf(stderr,"Firing point [%s] cannot be included",
                          temp_fp[i].id);
                        fprintf(stderr," as it refers to this weapon code\n");
                        error = 1;
                        break;
                    }
                    if (temp_fp[i].ptr[j].targ_id[0])
                    {
                        check = 0;
                        for (count=0; count<num_targets; count++)
                        {
                            if (strcmp(temp_fp[i].ptr[j].targ_id,
                              targets[count].id) == 0)
                                check = 1;
                        }
                        if (!check)
                        {
                            temp_fp[i].include[0] = 0;
                            fprintf(stderr,
                              "Target id [%s] is not included in the current list\n",
                              temp_fp[i].ptr[j].targ_id);
                            fprintf(stderr,"Firing point [%s] cannot be included",
                              temp_fp[i].id);
                            fprintf(stderr," as it refers to this target id\n");
                            error = 1;
                            break;
                        }
                    }
                }
                if (error)
                    continue;
            }
        }
    
        if (error)
        {
            printf("\n Hit return to continue...");
            G_gets(buf);
        }
    
        error = 0;
    
        for (i=0; i<num_wtable; i++)
        {
            if ( temp_fp[i].include[0] )
            {
                if (num_fp == MAX_FP)
                {
                    fprintf(stderr,"Error -- cannot add firing point [%s]\n",
                      temp_fp[i].id);
                    error = 1;
                    break;
                }
    
                new_firing_point();
                strcpy(firing[num_fp-1].id, temp_fp[i].id);
                firing[num_fp-1].easting = temp_fp[i].easting;
                firing[num_fp-1].northing = temp_fp[i].northing;
                firing[num_fp-1].gc_fact = temp_fp[i].gc_fact;
                firing[num_fp-1].num_weap = temp_fp[i].num_weap;
                firing[num_fp-1].ptr =
                  (FP_INFO *)G_calloc(firing[num_fp-1].num_weap,sizeof(FP_INFO));
    
                for (j=0; j<temp_fp[i].num_weap; j++)
                {
        /* MASSCOMP compiler bug requires us to copy to intermediate variables */
                    int ii;
                    float ff;
                    char *cc;
    
                    ii = temp_fp[i].ptr[j].weap_code;
                    firing[num_fp-1].ptr[j].weap_code = ii;
    
                    ii = temp_fp[i].ptr[j].num_day;
                    firing[num_fp-1].ptr[j].num_day = ii;
    
                    ii = temp_fp[i].ptr[j].num_night;
                    firing[num_fp-1].ptr[j].num_night = ii;
    
                    ii = temp_fp[i].ptr[j].min_charge;
                    firing[num_fp-1].ptr[j].min_charge = ii;
    
                    ii = temp_fp[i].ptr[j].max_charge;
                    firing[num_fp-1].ptr[j].max_charge = ii;
    
                    cc = temp_fp[i].ptr[j].targ_id;
                    strcpy(firing[num_fp-1].ptr[j].targ_id, cc);
    
                    cc = temp_fp[i].ptr[j].noise;
                    strcpy(firing[num_fp-1].ptr[j].noise, cc);
    
                    ff = temp_fp[i].ptr[j].height;
                    firing[num_fp-1].ptr[j].height = ff;
                }
    
            }
        }
    
        if (error)
        {
            fprintf(stderr,"  Maximum allowed number of firing points is %d\n",
              MAX_FP);
            fprintf(stderr,"  Limit has been reached\n");
            sleep(3);
        }
    }         /*  end section for "adding" begin section for "deleting" */
    else
    {
        for (i=0; i<num_wtable; i++)
        {
            if ( !temp_fp[i].include[0] )
            {
                new_firing_point();
                strcpy(firing[num_fp-1].id, temp_fp[i].id);
                firing[num_fp-1].easting = temp_fp[i].easting;
                firing[num_fp-1].northing = temp_fp[i].northing;
                firing[num_fp-1].gc_fact = temp_fp[i].gc_fact;
                firing[num_fp-1].num_weap = temp_fp[i].num_weap;
                firing[num_fp-1].ptr =
                  (FP_INFO *)G_calloc(firing[num_fp-1].num_weap,sizeof(FP_INFO));
    
                for (j=0; j<temp_fp[i].num_weap; j++)
                {
        /* MASSCOMP compiler bug requires us to copy to intermediate variables */
                    int ii;
                    float ff;
                    char *cc;
    
                    ii = temp_fp[i].ptr[j].weap_code;
                    firing[num_fp-1].ptr[j].weap_code = ii;
    
                    ii = temp_fp[i].ptr[j].num_day;
                    firing[num_fp-1].ptr[j].num_day = ii;
    
                    ii = temp_fp[i].ptr[j].num_night;
                    firing[num_fp-1].ptr[j].num_night = ii;
    
                    ii = temp_fp[i].ptr[j].min_charge;
                    firing[num_fp-1].ptr[j].min_charge = ii;
    
                    ii = temp_fp[i].ptr[j].max_charge;
                    firing[num_fp-1].ptr[j].max_charge = ii;
    
                    cc = temp_fp[i].ptr[j].targ_id;
                    strcpy(firing[num_fp-1].ptr[j].targ_id, cc);
    
                    cc = temp_fp[i].ptr[j].noise;
                    strcpy(firing[num_fp-1].ptr[j].noise, cc);
    
                    ff = temp_fp[i].ptr[j].height;
                    firing[num_fp-1].ptr[j].height = ff;
                }
    
            }
        }
    }
    
}
