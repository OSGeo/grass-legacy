#define EXTERN extern

#include "gis.h"
#include "water.h"


thin_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    char save_thin[31];
    int save_accthres, save_thinnum;
    char buf[80];
    int i;

    strcpy(save_thin, thin_name);
    save_accthres = acc_thres;
    save_thinnum = thin_num;

    strcpy(run,"no");

    if (complete[3] == 0)
    {
        fprintf(stderr,"Error -- step 3 (drainage accumulation) has not ");
        fprintf(stderr," been completed...unable to run this step.\n\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    if ((complete[3] == 1) && (river_name[0] == 0))
    {
        fprintf(stderr,"Error: recorded that step 3 (drainage accumulation)\n");
        fprintf(stderr," has been completed, but accumulation map layer\n");
        fprintf(stderr," name is not recorded\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    river_mapset = G_find_cell2(river_name,"");
    if (river_mapset == NULL)
    {
        fprintf(stderr,"Can't find accumulation map layer [%s]\n",river_name);
        fprintf(stderr,"Unable to run this step\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }


    V_clear();
    V_line(2,"PROJECT NAME:  ");
    V_const(proj_name,'s',2,17,40);

    V_line(4,step4);
    V_line(6,"Input:");
    V_line(7,"    Will use drainage accumulation map layer from Step 3");
    V_line(9,"Output:");
    V_line(10,"    Thinned network map layer:");
    V_line(11,"    Number of thinning iterations:");
    V_line(12,"    Accumulation threshold:");

    V_ques(thin_name,'s',10,46,30);
    V_ques(&thin_num,'i',11,46,7);
    V_ques(&acc_thres,'i',12,46,7);

    V_line(14,"            Run thin program?");

    V_ques(run,'s',14,31,3);

    while (1)
    {
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
        {
            strcpy(thin_name, save_thin);
            acc_thres = save_accthres;
            thin_num = save_thinnum;
            return;
        }

        if (thin_name[0] == 0)
        {
            fprintf(stderr,"Please provide requested map layer name\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        if ((acc_thres <= 0) || (thin_num <= 0))
        {
            fprintf(stderr,"Please enter positive integers for thinning iterations and threshold\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        break;
    }

    if (strncmp(run,"y",1) == 0)
    {
        thin_mapset = G_find_cell2(thin_name,G_mapset());
        if (thin_mapset != NULL)
        {
            printf("Thinned network map layer already exists\n");
            printf("  ok to overwrite? [y/n]  ");
            answer = get_answer();

            if (answer == 'n')
            {
                thin_mapset = G_ask_cell_new("Enter thinned network map layer",
                  thin_name);
                if (!thin_mapset)
                {
                    strcpy(thin_name, save_thin);
                    acc_thres = save_accthres;
                    thin_num = save_thinnum;
                    return;
                }
            }
        }

        for (i=4; i<7; i++)
            complete[i] = 0;

        sprintf(command,
            "%s/etc/Gthin -v 'accum=%s in %s' thin=%s iters=%d dthres=%d\n",
            G_gisbase(), river_name, river_mapset, thin_name, thin_num, 
            acc_thres);

        if (!system(command))
            complete[4] = 1;
        else
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
    else
    {
        if (strcmp(save_thin, thin_name) || (save_accthres != acc_thres)
          || (save_thinnum != thin_num))
        {
            for (i=4; i<7; i++)
                complete[i] = 0;
        }
    }
}
