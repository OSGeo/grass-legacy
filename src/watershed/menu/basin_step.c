#define EXTERN extern

#include "gis.h"
#include "water.h"


basin_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char save_basin[31];
    char buf[80];
    int i;

    strcpy(save_basin, basin_name);

    strcpy(run,"no");

    if (complete[5] == 0)
    {
        fprintf(stderr,"Error -- step 5 (channel segment coding) has");
        fprintf(stderr," not been completed...unable to run this step.\n\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    aspect_mapset = G_find_cell2(aspect_name,"");
    if (!aspect_mapset)
    {
        fprintf(stderr,"Adjusted pointer file [%s] not found\n",
          aspect_name);
        fprintf(stderr,"Unable to run this step\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    extthin_mapset = G_find_cell2(extthin_name,"");
    if (!extthin_mapset)
    {
        fprintf(stderr,"Extended stream network [%s] not found\n",
          extthin_name);
        fprintf(stderr,"Unable to run this step\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    if (G_get_cellhd(extthin_name,extthin_mapset,&window) < 0)
    {
        fprintf(stderr,"Problem with cell header for %s\n",extthin_name);
        fprintf(stderr,"Unable to run this step\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }
    else
        G_set_window(&window);


    V_clear();
    V_line(2,"PROJECT NAME:  ");
    V_const(proj_name,'s',2,17,40);

    V_line(4,step6);
    V_line(6,"Input:");
    V_line(7,"    Will use nodes file from Step 5.");
    V_line(9,"Output:");
    V_line(10,"    Subwatershed map layer:");

    V_ques(basin_name,'s',10,46,30);

    V_line(14,"            Run subbasin program?");

    V_ques(run,'s',14,35,3);


    while (1)
    {
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(basin_name, save_basin);
            return;
        }

        if (basin_name[0] == 0)
        {
            fprintf(stderr,"Please provide requested map layer name\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        break;
    }


    if (strncmp(run,"y",1) == 0)
    {
        basin_mapset = G_find_cell2(basin_name,G_mapset());
        if (basin_mapset != NULL)
        {
            printf("Subwatershed map layer already exists\n");
            printf("  ok to overwrite? [y/n]  ");
            answer = get_answer();

            if (answer == 'n')
            {
                basin_mapset = G_ask_cell_new("Enter subwatershed map layer name",
                  basin_name);
                if (!basin_mapset)
                {
                    strcpy(basin_name, save_basin);
                    return;
                }
            }
        }

        complete[6] = 0;
        sprintf(command,
            "%s/etc/Gbasins -v 'stream=%s in %s' 'drain=%s in %s' basin=%s\n",
            G_gisbase(), extthin_name, extthin_mapset, aspect_name,
            aspect_mapset, basin_name);

        if (!system(command))
            complete[6] = 1;
        else
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
    else
    {
        if (strcmp(save_basin,basin_name))
            complete[6] = 0;
    }
}
