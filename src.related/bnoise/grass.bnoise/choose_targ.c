#define EXTERN extern

#include "gis.h"
#include "edit.h"

choose_target(num_wtable,add_flag) int num_wtable; int add_flag;
{

    int i;
    int error;
    int count;
    int minus;
    char include[50][2];
    char buf[80];
    int weap;

/*  add_flag is variable used to denote if target points are being
    added from a table of target points (add_flag=1) or deleted
    from the current list (add_flag=0)  */

    for (i=0; i<MAX_TARG; i++)
        include[i][0] = 0;

    V_clear();
    if (add_flag)
    {
        V_line(2,"    PLACE AN 'x' BESIDE EACH TARGET YOU WISH TO INCLUDE");
        V_line(4,"Include?    Target ID:     Easting:     Northing:");
    }
    else
    {
        V_line(2,"    PLACE AN 'x' BESIDE EACH TARGET YOU WISH TO DELETE");
        V_line(4,"Delete?     Target ID:     Easting:     Northing:");
    }

    minus = 0;

    for (count=0; count<num_wtable; count++)
    {
        V_line(6+(count-minus)," ___");
        V_ques(include[count],'s',6+(count-minus),2,1);
        V_const(temp_targ[count].id,'s',6+(count-minus),16,3);
        V_const(&temp_targ[count].easting,'d',6+(count-minus),25,11);
        V_const(&temp_targ[count].northing,'d',6+(count-minus),37,11);

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
                V_line(2,"    PLACE AN 'x' BESIDE EACH TARGET YOU WISH TO INCLUDE");
                V_line(4,"Include?    Target ID:     Easting:     Northing:");
            }
            else
            {
                V_line(2,"    PLACE AN 'x' BESIDE EACH TARGET YOU WISH TO DELETE");
                V_line(4,"Delete?     Target ID:     Easting:     Northing:");
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
        error = 0;

        for (i=0; i<num_wtable; i++)
        {
            if ( include[i][0] )
            {
                for (count=0; count<num_targets; count++)
                {
                    if (strcmp(temp_targ[i].id,targets[count].id) == 0)
                    {
                        include[i][0] = 0;
                        fprintf(stderr,
                          "Target id [%s] already used, cannot be reused\n",
                          temp_targ[i].id);
                        error = 1;
                        break;
                    }
                }
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
            if ( include[i][0] )
            {
                if (num_targets == MAX_TARG)
                {
                    fprintf(stderr,"Error -- cannot add target [%s]\n",
                      temp_targ[i].id);
                    error = 1;
                    break;
                }
                strcpy(targets[num_targets].id,temp_targ[i].id);
                targets[num_targets].easting = temp_targ[i].easting;
                targets[num_targets].northing = temp_targ[i].northing;
                targets[num_targets].gc_fact = temp_targ[i].gc_fact;
                num_targets++;
            }
        }

        if (error)
        {
            fprintf(stderr,"  Maximum allowed number of target points is %d\n",
              MAX_TARG);
            fprintf(stderr,"  Limit has been reached\n");

            printf("\n Hit return to continue...");
            G_gets(buf);
        }
    }   /* end "if adding" begin "if deleting"  */
    else
    {
         error = 0;

/*  if firing points exist, check the target points being "used" by
    them before allowing the deletion of that target point  */

        if (num_fp)
        {
            for (i=0; i<num_wtable; i++)
            {
                if ( include[i][0] )     /* if included for deletion */
                {
                    for (count=0; count<num_fp; count++)
                    {
                        for (weap=0; weap<firing[count].num_weap; weap++)
                        {
                            if (strcmp(firing[count].ptr[weap].targ_id,
                              temp_targ[i].id) == 0)
                            {
                            fprintf(stderr,
                              "\nCan't delete target [%s] as it is used by",
                              temp_targ[i].id);
                            fprintf(stderr,
                              " firing point [%s]\n",firing[count].id);
                            error = 1;
                            include[i][0] = 0;
                            }
                        }
                    }
                }
            }
        }

        if (error)
        {
            printf("\nHit return to continue...");
            G_gets(buf);
        }

/* save in current list those NOT chosen for deletion  */

        for (i=0; i<num_wtable; i++)
        {
            if ( !include[i][0] )
            {

                strcpy(targets[num_targets].id,temp_targ[i].id);
                targets[num_targets].easting = temp_targ[i].easting;
                targets[num_targets].northing = temp_targ[i].northing;
                targets[num_targets].gc_fact = temp_targ[i].gc_fact;
                num_targets++;
            }
        }
    }
}
