#define EXTERN extern

#include "menu.h"


partition_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char save_part[31];
    char buf[80];

    strcpy(save_part, part_name);

    strcpy(run,"no");

    if (G_get_cellhd(basin_name,basin_mapset,&window) < 0)
    {
        fprintf(stderr,"Problem with cell header for [%s]\n",basin_name);
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

    V_line(4,step1);
    V_line(6,"Input:");
    V_line(7,"    Will use subbasin and extended stream network");
    V_line(8,"      map layers from watershed project");
    V_line(10,"Output:");
    V_line(11,"    Partitioned map layer:");

    V_ques(part_name,'s',11,46,30);

    V_line(14,"            Run partition program?");

    V_ques(run,'s',14,35,3);


    while (1)
    {
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(part_name, save_part);
            return;
        }

        if (part_name[0] == 0)
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
        part_mapset = G_find_cell(part_name,"");
        if (part_mapset != NULL)
        {
            printf("Partition map layer already exists\n");
            printf("  ok to overwrite? [y/n]  ");
            answer = get_answer();
        }

        if (answer == 'n')
        {
            part_mapset = G_ask_cell_new("Enter partition map layer",
              part_name);
            if (!part_mapset)
            {
                strcpy(part_name, save_part);
                return;
            }
        }

        sprintf(command,
            "%s/etc/Gpartition %s %s %s\n",
            G_gisbase(), basin_name, extthin_name, part_name);

        if (system(command))
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
	    printf("\nHit return to continue...");
	    G_gets(buf);
        }
    }
}
