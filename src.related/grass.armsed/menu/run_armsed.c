#define EXTERN extern

#include "menu.h"


run_armsed()
{

    char answer;
    char get_answer();
    char command[1024];
    char dir[80];
    char buf[25];
    int error;
    char path[1024];

    if (part_name[0] == 0)
    {
        fprintf(stderr,"No partition map layer name recorded.\n");
        fprintf(stderr,"Want to enter name now?");
        answer = get_answer();

        if (answer == 'y')
        {
            G_ask_any("Enter partitioned map layer", part_name,
              "cell","cell","");
        }
        else
            return;
    }

    error = 0;

    if ( !G_find_file("multsed/input",part_name,""))
    {
        fprintf(stderr,
          "No armsed input files found for partitioned map layer [%s]\n",
          part_name);
        fprintf(stderr,"Cannot run armsed\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    sprintf(dir,"multsed/input/%s",part_name);

    if (sim_title[0] == 0)
    {
        sim_mapset = G_ask_old("Enter simulation name",sim_title,
          dir,"simulation input");
        if (!sim_mapset)
            return;
    }

    sprintf(path,"%s/%s/%s/%s/%s", G_gisdbase(), G_location(), sim_mapset,
      dir, sim_title);

    strcat(dir,"/");
    strcat(dir,sim_title);

    if ( !G_find_file(dir,"tape1",sim_mapset))
    {
        error = 1;
        fprintf(stderr, "armsed input file tape1 missing\n");
    }
    if ( !G_find_file(dir,"tape2",sim_mapset))
    {
        error = 1;
        fprintf(stderr, "armsed input file tape2 missing\n");
    }
    if ( !G_find_file(dir,"tape9",sim_mapset))
    {
        error = 1;
        fprintf(stderr, "armsed input file tape9 missing\n");
    }
    if ( !G_find_file(dir,"tape10",sim_mapset))
    {
        error = 1;
        fprintf(stderr, "armsed input file tape10 missing\n");
    }
    if (error)
    {
        fprintf(stderr,"Cannot run armsed\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    sprintf(command,"cd %s; armsed1; armsed2; armsed3", path);
    system(command);

}
