#define EXTERN extern

#include "menu.h"


input_step()
{

    char answer;
    char get_answer();
    char command[1024];
    char buf[80];
    int option;
    int old_flag=0;
    char dir[80];
    char *mapset;

    if ((extthin_name[0] == 0) || (part_name[0] == 0))
    {
        fprintf(stderr,"Cannot run this step\n");
            fprintf(stderr,"  streams map layer or partition map layer name missing\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            return;
        }
/*
    while(1)
    {
        V_clear();

        V_line(3, " Choose option for creating simulation input files:");
        V_line(4, "  (input number of your choice)");

        V_line(7, " 1.  Create a new simulation");
        V_line(8, " 2.  Modify an old simulation");

        V_line(11, " Desired option:");

        V_ques(&option,'i',11,18,3);

        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
            return;

        if ((option < 1) || (option > 2))
        {
            fprintf(stderr,"Enter number between 1 and 2 inclusive\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        if ((option == 1) && ((extthin_name[0] == 0) || (part_name[0] == 0)))
        {
            fprintf(stderr,"Cannot choose option 1\n");
            fprintf(stderr,"  streams map layer or partition map layer name missing\n");
            option = 0;
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }
        if (option == 2)
        {
            if (!G_find_file2("multsed/input",part_name,""))
            {
                fprintf(stderr,"Cannot choose option 2\n");
                fprintf(stderr,
                  "Unable to find any existing simulation files for division map layer [%s]\n",
                  part_name);
                option = 0;
                printf("\nHit return to continue...");
                G_gets(buf);
                continue;
            }
        }
        break;
    }

    if (option == 2)
        old_flag = 1;

    if (old_flag)
    {
        sprintf(dir,"multsed/input/%s", part_name);
        sim_mapset = G_ask_old("Enter old simulation name",sim_title,
          dir,"simulation input");
        if (!sim_mapset)
            return;
    }
    else
    {

*/
        sprintf(dir,"multsed/input/%s", part_name);

        mapset = G_find_file2("watershed/drain",extthin_name,"");
        if (!mapset)
        {
            fprintf(stderr,"Error -- drainage file missing for %s\n",extthin_name);
            fprintf(stderr,"  cannot run simulation\n");

            printf("\nHit return to continue...");
            G_gets(buf);
            return;
        }

        mapset = G_find_file2("multsed/stats",part_name,"");
        if (!mapset)
        {
            fprintf(stderr,"Error -- stats file missing for %s\n",part_name);
            fprintf(stderr,"  run statistics collection routine and try again\n");

            printf("\nHit return to continue...");
            G_gets(buf);
            return;
        }

        sprintf(dir,"multsed/input/%s", part_name);

        sim_mapset = G_ask_new("Enter new simulation name",sim_title,dir,"simulation input");
        if (!sim_mapset)
            return;

/*    } */

    if (old_flag)
    {
        sprintf(command,"%s/etc/Garmsed.input -o 'sim=%s in %s'\n",
          G_gisbase(), sim_title, sim_mapset);
    }
    else
    {
        sprintf(command,
          "%s/etc/Garmsed.input -n 'streams=%s in %s' 'div=%s in %s' sim=%s\n",
        G_gisbase(), extthin_name, extthin_mapset, part_name, part_mapset,
        sim_title);
    }

    if (system(command))
    {
        fprintf(stderr,"WARNING:  Bad exit status -- step not completed\n");
        printf("\nHit return to continue...");
        G_gets(buf);
    }
}
