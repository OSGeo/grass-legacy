#define EXTERN extern

#include "gis.h"
#include "edit.h"

ask_weapon(num_wtable,add_flag) int num_wtable; int add_flag;
{

    int i,j;
    int minus;
    int weap_count;
    int error;
    char buf[80];

    V_clear();
    if (add_flag)
    {
        V_line(2,"    PLACE AN 'x' BESIDE EACH WEAPON TYPE YOU WISH TO INCLUDE");
        V_line(4,"Include?    Code:             Weapon Name:");
    }
    else
    {
        V_line(2,"    PLACE AN 'x' BESIDE EACH WEAPON TYPE YOU WISH TO DELETE");
        V_line(4,"Delete?     Code:             Weapon Name:");
    }

    minus = 0;

    for (weap_count=0; weap_count<num_wtable; weap_count++)
    {
        V_line(6+(weap_count-minus)," ___");
        V_ques(weapons[weap_count].include,'s',6+(weap_count-minus),2,1);
        V_const(&weapons[weap_count].code,'i',6+(weap_count-minus),5,2);
        V_const(weapons[weap_count].name,'s',6+(weap_count-minus),9,60);

        if (((weap_count+1)/15) * 15 == (weap_count+1))
        {

            minus += 15;

            if (add_flag)
                V_intrpt_ok();
            if ( !V_call() )
            {
                for (i=0; i<num_wtable; i++)
                    weapons[i].include[0] = 0;
                return;
            }

            V_clear();
            if (add_flag)
            {
                V_line(2,"    PLACE AN 'x' BESIDE EACH WEAPON TYPE YOU WISH TO INCLUDE");
                V_line(4,"Include?    Code:             Weapon Name:");
            }
            else
            {
                V_line(2,"    PLACE AN 'x' BESIDE EACH WEAPON TYPE YOU WISH TO DELETE");
                V_line(4,"Delete?     Code:             Weapon Name:");
            }

        }
    }

    if ((num_wtable/15)*15 != num_wtable)
    {
        if (add_flag)
            V_intrpt_ok();
        if ( !V_call() )
        {
            for (i=0; i<num_wtable; i++)
                weapons[i].include[0] = 0;
            return;
        }
    }

    weap_count = 0;
    error = 0;

    if (add_flag)
    {
        for (i=0; i<num_wtable; i++)
        {
            if ( weapons[i].include[0] )
            {
                if ( (weap_codes[weapons[i].code]/3)*3  ==
                    weap_codes[weapons[i].code] )
                {
                    error = 0;
                    weapons[i].include[0] = 0;
    fprintf(stderr,"Weapon code %d already chosen, will not be added to list\n",
       weapons[i].code);
                }
                else
                {
                    weap_codes[weapons[i].code] *= 3;
                    weap_count++;
                }
            }
        }
        if (error)
        {
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }      /* end of "if adding" and begin of "if deleting"  */
    else
    {
        error = 0;

        if (num_fp)
        {
            for (i=0; i<num_wtable; i++)
            {
                if ( weapons[i].include[0] )    /* if included for deletion */
                {
                    for (j=0; j<num_fp; j++)
                    {
                        for (weap_count=0; weap_count<firing[j].num_weap; weap_count++)
                        {
                            if (weapons[i].code ==
                              firing[j].ptr[weap_count].weap_code)
                            {
                                fprintf(stderr, "\nCan't delete weapon\n  [%s]",
                                  weapons[i].name);
                                fprintf(stderr,
                                  "\nIt is being used by firing point [%s]\n",
                                  firing[j].id);
                                error = 1;
                                weapons[i].include[0] = 0;
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
        }

        weap_count = 0;

        for (i=0; i<num_wtable; i++)
        {
/* if chosen for deletion, mark as not chosen, and vice versa, so that
   those which were originally "not chosen" (for deletion) will be
   written back to file in write_weap  */

            if ( weapons[i].include[0] )    /* if included for deletion */
                weapons[i].include[0] = 0;
            else
            {
                sprintf(weapons[i].include,"x");
                weap_codes[weapons[i].code] *= 3;
                weap_count++;
            }
        }
    }

    return weap_count;

}
