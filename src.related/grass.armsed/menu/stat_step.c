#define EXTERN extern

#include "menu.h"


stat_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char comm2[100];
    char save_soils[41];
    char save_cover[41];
    char save_part[41];
    char buf[80];

    strcpy(save_soils, soils_name);
    strcpy(save_cover, cover_name);
    strcpy(save_part, part_name);

    strcpy(run,"no");

    if (part_name[0] == 0)
    {
        fprintf(stderr,"Error: partitioned map layer name is blank\n");
        fprintf(stderr,"Run step one (partitioning) and try this step again\n");
        printf("\nHit return to continue...");
        G_gets(buf);
    }


    V_clear();
    V_line(2,"PROJECT NAME:  ");
    V_const(proj_name,'s',2,17,40);

    V_line(4,step2);
    V_line(6,"Input:");
    V_line(7,"    Will use elevation map layer from watershed project");
    V_line(8,"    Partitioned map layer:");
    V_line(9,"     (from step 1. or renumbered version)");
    V_line(11,"    Soils map layer (optional):");
    V_line(12,"    Cover map layer (optional):");
    V_line(13,"Output:");
    V_line(14,"    Will create stats file under element multsed/stats,");
    V_line(15,"    with name of partitioned map layer.");
    V_line(18,"            Run stats program?");

    V_ques(part_name,'s',8,46,30);
    V_ques(soils_name,'s',11,46,30);
    V_ques(cover_name,'s',12,46,30);

    V_ques(run,'s',18,31,3);


    while (1)
    {
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(soils_name, save_soils);
            strcpy(cover_name, save_cover);
            strcpy(part_name, save_part);
            return;
        }
    
        break;
    }

    if (strncmp(run,"y",1) == 0)
    {
        if (soils_name[0])
        {
            G_strip(soils_name);
            soils_mapset = G_find_cell2(soils_name,"");
            if (!soils_mapset)
            {
                fprintf(stdout,"Soils map layer [%s] not found\n\n",
                  soils_name);
                soils_mapset = G_ask_cell_old("Enter soils map layer:",
                  soils_name);
        
                if (!soils_mapset)
                {
                    strcpy(soils_name, save_soils);
                    strcpy(cover_name, save_cover);
                    strcpy(part_name, save_part);
                    return;
                }
            }
        }

        if (cover_name[0])
        {
            G_strip(cover_name);
            cover_mapset = G_find_cell2(cover_name,"");
            if (!cover_mapset)
            {
                fprintf(stdout,"Cover map layer [%s] not found\n\n",
                  cover_name);
                cover_mapset = G_ask_cell_old("Enter cover map layer:",
                  cover_name);
    
                if (!cover_mapset)
                {
                    strcpy(soils_name, save_soils);
                    strcpy(cover_name, save_cover);
                    strcpy(part_name, save_part);
                    return;
                }
            }
        }

        sprintf(command,
          "%s/etc/Garmsed.stats -w 'elev=%s in %s' 'div=%s in %s' 'stream=%s in %s'",
            G_gisbase(), elev_name, elev_mapset, part_name, part_mapset,
            extthin_name, extthin_mapset);

        if (soils_name[0] != 0)
        {
            sprintf(comm2," 'soils=%s in %s'", soils_name, soils_mapset);
            strcat(command,comm2);
        }

        if (cover_name[0] != 0)
        {
            sprintf(comm2," 'cover=%s in %s'", cover_name, cover_mapset);
            strcat(command,comm2);
        }

        strcat(command,"\n");

        if (system(command))
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
}
