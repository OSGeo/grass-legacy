#define EXTERN extern

#include "gis.h"
#include "water.h"


pits_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char save_filt[31];
    char buf[80];
    int i;

    strcpy(save_filt, filt_name);

    strcpy(run,"no");

    if ((complete[1] == 1) && (filt_name[0] == 0))
    {
        fprintf(stderr,"Error: recorded that filtering has been completed,\n");
        fprintf(stderr," but filtered elevation map layer is not reported\n");
        fprintf(stderr," Cannot run this step.\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    if (complete[1] == 0)
    {
        fprintf(stderr,"Warning -- step 1 (filtering of elevation data) ");
        fprintf(stderr," has not been completed\n\n");
        fprintf(stderr," want to use unfiltered data for this project?  ");
        answer = get_answer();

        if (answer == 'n')
            return;
        else
            *filt_name = 0;
    }


    V_clear();
    V_line(2,"PROJECT NAME:  ");
    V_const(proj_name,'s',2,17,40);

    V_line(4,step2);
    V_line(6,"Input:");
    if (filt_name[0] == 0)
        V_line(7,"   Enter elevation map layer:");
    else
        V_line(7,"    Will use filtered elevation data from Step 1.");
    V_line(9,"Output:");
    V_line(10,"    Will create pits file under element watershed/pits,");
    V_line(11,"    with name of [filtered] elevation data.");
    V_line(14,"            Run pits program?");

    if (filt_name[0] == 0)
        V_ques(filt_name,'s',7,46,30);
    V_ques(run,'s',14,31,3);


    while (1)
    {
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(filt_name, save_filt);
            return;
        }
    
        if (filt_name[0] == 0)
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
        filt_mapset = G_find_cell2(filt_name,"");
        if (!filt_mapset)
        {
            fprintf(stdout,"Elevation data map layer [%s] not found\n\n",
              filt_name);
            filt_mapset = G_ask_cell_old("Enter elevation data map layer:",
              filt_name);
    
            if (!filt_mapset)
            {
                strcpy(filt_name, save_filt);
                return;
            }
        }

        for (i=2; i<7; i++)
            complete[i] = 0;

        sprintf(command,"%s/etc/Gpits -v 'elev=%s in %s'\n",
            G_gisbase(), filt_name, filt_mapset);

        if (!system(command))
            complete[2] = 1;
        else
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
    else
    {
        if (strcmp(save_filt,filt_name))
        {
            for (i=2; i<7; i++)
                complete[i] = 0;
        }
    }
}
