#define EXTERN extern

#include "gis.h"
#include "water.h"

remove_files()
{

    char *mapset;
    char name[30];
    int i;
    char remove[3][2];
    int ret_num;

    for (i=0; i<3; i++)
        remove[i][0] = 0;

    V_clear();
    V_line(2,"    PLACE AN 'x' BESIDE EACH DESIRED OPTION");

    for (i=0; i<3; i++)
        V_ques(remove[i],'s',6+i,2,1);

    V_line(6," ___  Remove project files");
    V_line(7," ___  Remove pit point files");
    V_line(8," ___  Remove coded segment files");

    V_intrpt_ok();
    V_intrpt_msg("RETURN TO MAIN MENU");

    if ( !V_call() )
        return;

    if (remove[0][0])
    {
        mapset = G_find_file("watershed","project","");
        if (!mapset)
        {
            fprintf(stderr,
              "Impossible to remove project files -- none found\n");
        }
        else
        {
            printf("\nREMOVING PROJECT FILES...\n");
            while(1)
            {
                mapset = G_ask_old("enter project file name",name,
                  "watershed/project","parameter storing");
                if (!mapset)
                    break;

                ret_num = G_remove("watershed/project",name);
                if (!ret_num)
                {
                    fprintf(stderr,"Unable to remove project file [%s]\n\n",
                        name);
                    continue;
                }
            }
        }
    }

    if (remove[1][0])
    {
        mapset = G_find_file("watershed","pits","");
        if (!mapset)
        {
            fprintf(stderr,
              "Impossible to remove pit points files -- none found\n");
        }
        else
        {
            printf("\nREMOVING PIT POINTS FILES...\n");
            while(1)
            {
                mapset = G_ask_old("enter pit points file name",name,
                  "watershed/pits","pit points storing");
                if (!mapset)
                    break;

                ret_num = G_remove("watershed/pits",name);
                if (!ret_num)
                {
                    fprintf(stderr,"Unable to remove pit points file [%s]\n\n",
                        name);
                    continue;
                }
            }
        }
    }

    if (remove[2][0])
    {
        mapset = G_find_file("watershed","nodes","");
        if (!mapset)
        {
            fprintf(stderr,
              "Impossible to remove coded segment files -- none found\n");
        }
        else
        {

            printf("\nREMOVING CODED SEGMENT FILES...\n");
            while(1)
            {
                mapset = G_ask_old("enter coded segment file name",name,
                  "watershed/nodes","coded segment storing");
                if (!mapset)
                    break;

                ret_num = G_remove("watershed/nodes",name);
                if (!ret_num)
                {
                    fprintf(stderr,"Unable to remove coded segment file [%s]\n\n",
                        name);
                    continue;
                }
            }
        }
    }

}
