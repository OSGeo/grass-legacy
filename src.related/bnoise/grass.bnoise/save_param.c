
#define EXTERN extern

#include "gis.h"
#include "edit.h"

save_param()
{

    int i;
    char save[3][2];

    for (i=0; i<3; i++)
        save[i][0] = 0;

    V_clear();
    V_line(2,"    PLACE AN 'X' BESIDE EACH DESIRED OPTION");

    for (i=0; i<3; i++)
        V_ques(save[i],'s',6+i,2,1);

    V_line(6," ___  Save current weapons list");
    V_line(7," ___  Save current target points list");
    V_line(8," ___  Save current firing points list");

    V_intrpt_ok();
    V_intrpt_msg("TO RETURN TO MAIN MENU");

    if ( !V_call() )
        return;

    if (save[0][0])
    {
        if (num_weapons == 0)
        {
            fprintf(stderr,
              "Impossible to save weapons -- number of chosen weapons is 0\n");
            save[0][0] = 0;
        }
    }

    if (save[1][0])
    {
        if (num_targets == 0)
        {
            fprintf(stderr,
              "Impossible to save targets -- number of chosen targets is 0\n");
            save[1][0] = 0;
        }
    }

    if (save[2][0])
    {
        if (num_fp == 0)
        {
            fprintf(stderr,
              "Impossible to save firing points -- number of chosen firing points is 0\n");
            save[2][0] = 0;
        }
    }

    if (save[0][0])
    {
        while(1)
        {
            gun_mapset = G_ask_any("enter weapons file name",gun_name,
              "noise/weapons","weapons storing",1);
            if (!gun_mapset)
                break;

            gun_fd = G_fopen_new("noise/weapons",gun_name);
            if (!gun_fd)
            {
                fprintf(stderr,"Error opening new weapons file [%s]\n",
                  gun_name);
                sleep(3);
                break;
            }

            weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
            temp1_fd = fopen(temp1_name,"r");
            read_weapon(temp1_fd,0,num_weapons,0);
            fclose(temp1_fd);

            fprintf(gun_fd,"%d\n",num_weapons);
            write_weapon(gun_fd,num_weapons,1);
            fclose(gun_fd);
            free(weapons);
            break;
        }
    }

    if (save[1][0])
    {
        while(1)
        {
            targ_mapset = G_ask_any("enter target file name",targ_name,
              "noise/targets","target storing",1);
            if (!targ_mapset)
                break;

            targ_fd = G_fopen_new("noise/targets",targ_name);
            if (!targ_fd)
            {
                fprintf(stderr,"Unable to open new target file [%s]\n",
                  targ_name);
                sleep(3);
                break;
            }

            write_target(targ_fd);
            fclose(targ_fd);
            break;
        }
    }

    if (save[2][0])
    {
        while(1)
        {
            fp_mapset = G_ask_any("enter firing point file name",fp_name,
              "noise/firings","firing point storing",1);
            if (!fp_mapset)
                break;

            fp_fd = G_fopen_new("noise/firings",fp_name);
            if (!fp_fd)
            {
                fprintf(stderr,"Unable to open new firing point file [%s]\n",
                  fp_name);
                sleep(3);
                break;
            }

            fprintf(fp_fd,"%d\n",num_fp);
            write_fp(fp_fd);
            fclose(fp_fd);
            break;
        }
    }

}
