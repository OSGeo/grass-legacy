
#define EXTERN extern

#include "gis.h"
#include "edit.h"

remove_param()
{

    int i;
    char remove[3][2];
    int ret_num;

    for (i=0; i<3; i++)
        remove[i][0] = 0;

    V_clear();
    V_line(2,"    PLACE AN 'X' BESIDE EACH DESIRED OPTION");

    for (i=0; i<3; i++)
        V_ques(remove[i],'s',6+i,2,1);

    V_line(6," ___  Remove weapons storing files");
    V_line(7," ___  Remove target point files");
    V_line(8," ___  Remove firing point files");

    V_intrpt_ok();
    V_intrpt_msg("TO RETURN TO MAIN MENU");

    if ( !V_call() )
        return;

    if (remove[0][0])
    {
        gun_mapset = G_find_file("noise","weapons","");
        if (!gun_mapset)
        {
            fprintf(stderr,
              "Impossible to remove weapons files -- none found\n");
            remove[0][0] = 0;
        }
    }

    if (remove[1][0])
    {
        targ_mapset = G_find_file("noise","targets","");
        if (!targ_mapset)
        {
            fprintf(stderr,
              "Impossible to remove target point files -- none found\n");
            remove[1][0] = 0;
        }
    }

    if (remove[2][0])
    {
        fp_mapset = G_find_file("noise","firings","");
        if (!fp_mapset)
        {
            fprintf(stderr,
              "Impossible to remove firing point files -- none found\n");
            remove[2][0] = 0;
        }
    }

    if (remove[0][0])
    {
        printf("\nREMOVING WEAPONS FILES...\n");
        while(1)
        {
            gun_mapset = G_ask_old("enter weapons file name",gun_name,
              "noise/weapons","weapons storing");
            if (!gun_mapset)
                break;

            ret_num = G_remove("noise/weapons",gun_name);
            if (!ret_num)
            {
                fprintf(stderr,"Unable to remove weapons file [%s]\n\n",
                    gun_name);
                continue;
            }
        }
    }

    if (remove[1][0])
    {
        printf("\nREMOVING TARGET FILES...\n");
        while(1)
        {
            targ_mapset = G_ask_old("enter target file name",targ_name,
              "noise/targets","target storing");
            if (!targ_mapset)
                break;

            ret_num = G_remove("noise/targets",targ_name);
            if (!ret_num)
            {
                fprintf(stderr,"Unable to remove target point file [%s]\n\n",
                    targ_name);
                continue;
            }
        }
    }

    if (remove[2][0])
    {
        printf("\nREMOVING FIRING POINT FILES...\n");
        while(1)
        {
            fp_mapset = G_ask_old("enter firing point file name",fp_name,
              "noise/firings","firing point storing");
            if (!fp_mapset)
                break;

            ret_num = G_remove("noise/firings",fp_name);
            if (!ret_num)
            {
                fprintf(stderr,"Unable to remove firing point file [%s]\n\n",
                    fp_name);
                continue;
            }
        }
    }

}
