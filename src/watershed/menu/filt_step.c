#define EXTERN extern

#include "gis.h"
#include "water.h"


filt_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char save_elev[31], save_filt[31];
    int save_num;
    char buf[80];
    int i;

    strcpy(save_elev, elev_name);
    strcpy(save_filt, filt_name);
    save_num = filt_num;

    strcpy(run,"no");

    V_clear();
    V_line(2,"PROJECT NAME:  ");
    V_const(proj_name,'s',2,17,40);

    V_line(4,step1);
    V_line(6,"Input:");
    V_line(7,"    Unscaled elevation map layer:");
    V_line(9,"Output:");
    V_line(10,"    Filtered elevation map layer:");
    V_line(11,"    Number of filter iterations:");
    V_line(14,"            Run filter program?");

    V_ques(elev_name,'s',7,46,30);
    V_ques(filt_name,'s',10,46,30);
    V_ques(&filt_num,'i',11,46,5);
    V_ques(run,'s',14,33,3);

    while (1)
    {
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(elev_name, save_elev);
            strcpy(filt_name, save_filt);
            filt_num = save_num;
            return;
        }
    
        if (filt_num <= 0)
        {
            fprintf(stderr,"Please enter positive number of filter iterations\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        if ((elev_name [0] == 0) || (filt_name[0] == 0))
        {
            fprintf(stderr,"Please provide requested map layer names\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        break;
    }

    if (strncmp(run,"y",1) == 0)
    {
        elev_mapset = G_find_cell2(elev_name,"");
        if (!elev_mapset)
        {
            fprintf(stdout,"Elevation map layer [%s] not found\n\n",elev_name);
            elev_mapset = G_ask_cell_old("Enter unscaled elevation map layer:",
              elev_name);
    
            if (!elev_mapset)
            {
                strcpy(elev_name, save_elev);
                strcpy(filt_name, save_filt);
                filt_num = save_num;
                return;
            }
        }

        filt_mapset = G_find_cell2(filt_name,G_mapset());
        if (filt_mapset != NULL)
        {
            printf("Filtered elevation map layer already exists\n");
            printf("  ok to overwrite? [y/n]  ");
            answer = get_answer();

            if (answer == 'n')
            {
                filt_mapset = G_ask_cell_new(
                  "Enter filtered elevation map layer name", filt_name);
                if (!filt_mapset)
                {
                    strcpy(elev_name, save_elev);
                    strcpy(filt_name, save_filt);
                    filt_num = save_num;
                    return;
                }
            }
        }

        for (i=1; i<7; i++)
            complete[i] = 0;

        sprintf(command,
            "%s/etc/Gfiltermap -v 'in=%s in %s' out=%s passes=%d\n",
            G_gisbase(), elev_name, elev_mapset, filt_name, filt_num);

        if (!system(command))
            complete[1] = 1;
        else
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
    else
    {
        if (strcmp(save_elev,elev_name) || strcmp(save_filt,filt_name) ||
          (save_num != filt_num))
        {
            for (i=1; i<7; i++)
                complete[i] = 0;
        }
    }

}
