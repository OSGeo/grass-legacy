#define EXTERN extern

#include "gis.h"
#include "water.h"


code_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char save_extname[31], save_clname[31];
    char buf[80];
    int i;

    strcpy(save_extname, extthin_name);
    strcpy(save_clname, clthin_name);

    strcpy(run,"no");

    if (complete[4] == 0)
    {
        fprintf(stderr,"Error -- step 4 (thinning of accumulation map layer) ");
        fprintf(stderr," has not been completed...unable to run this step.\n\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    aspect_mapset = G_find_cell2(aspect_name,"");
    if (!aspect_mapset)
    {
        fprintf(stderr,"Adjusted pointer map layer [%s] not found\n",
          aspect_name);
        fprintf(stderr,"Unable to run this step\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    river_mapset = G_find_cell2(river_name,"");
    if (!river_mapset)
    {
        fprintf(stderr,"Drainage accumulation map layer [%s] not found\n",
          river_name);
        fprintf(stderr,"Unable to run this step\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    if (G_get_cellhd(river_name,river_mapset,&window) < 0)
    {
        fprintf(stderr,"Problem with cell header for [%s]\n",river_name);
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

    V_line(4,step5);
    V_line(6,"Input:");
    V_line(7,"    Extended thinned network map layer:");
    V_line(9,"Output:");
    V_line(10,"    Coded network map layer (optional):");
    V_line(11,"    Nodes file will have extended network name");

    V_ques(extthin_name,'s',7,46,30);
    V_ques(clthin_name,'s',10,46,30);

    V_line(14,"            Run code program?");

    V_ques(run,'s',14,33,3);

    while (1)
    {
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(extthin_name, save_extname);
            strcpy(clthin_name, save_clname);
            return;
        }

        if (extthin_name[0] == 0)
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
        extthin_mapset = G_find_cell2(extthin_name,"");
        if (!extthin_mapset)
        {
            fprintf(stderr,"Extended network file [%s] not found\n",
              extthin_name);
            extthin_mapset = G_ask_cell_old("Enter extended network map layer name",
              extthin_name,"");
            if (!extthin_mapset)
            {
                strcpy(extthin_name, save_extname);
                strcpy(clthin_name, save_clname);
                return;
            }
        }

        if (clthin_name[0] != 0)
        {
            clthin_mapset = G_find_cell2(clthin_name,G_mapset());
            if (clthin_mapset != NULL)
            {
                printf("Coded network map layer already exists\n");
                printf("   ok to overwrite? [y/n]  ");
                answer = get_answer();
                if (answer == 'n')
                {
                    clthin_mapset = G_ask_cell_new(
                      "Enter coded network map layer", clthin_name);
                    if (!clthin_mapset)
                    {
                        strcpy(extthin_name, save_extname);
                        strcpy(clthin_name, save_clname);
                        return;
                    }
                }
            }
        }


        for (i=5; i<7; i++)
            complete[i] = 0;

        sprintf(command,
            "%s/etc/Gclcode -v 'accum=%s in %s' 'stream=%s in %s' 'drain=%s in %s'",
            G_gisbase(), river_name, river_mapset, extthin_name, extthin_mapset,
            aspect_name, aspect_mapset);

        if (clthin_name[0] != 0)
        {
            strcat(command," out=");
            strcat(command,clthin_name);
        }

        strcat(command,"\n");

        if (!system(command))
            complete[5] = 1;
        else
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
    else
    {
        if (strcmp(save_extname, extthin_name) ||
          strcmp(save_clname, clthin_name))
        {
            for (i=5; i<7; i++)
                complete[i] = 0;
        }
    }

}
