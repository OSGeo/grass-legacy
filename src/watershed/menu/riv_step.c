#define EXTERN extern

#include "gis.h"
#include "water.h"


riv_step()
{

    char answer;
    char get_answer();
    char run[4];
    char command[1024];
    double save_east, save_north;
    int save_pit;
    char save_river[31], save_aspect[31], save_lakes[31];
    char buf[80];
    int i;

    save_east = easting;
    save_north = northing;
    save_pit = pit_thres;
    strcpy(save_river, river_name);
    strcpy(save_aspect, aspect_name);
    strcpy(save_lakes, lakes_name);

    strcpy(run,"no");

    if (complete[2] == 0)
    {
        fprintf(stderr,"Step 2 (pits locating) has not been completed.\n");
        fprintf(stderr," Cannot run this step.\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    if ((complete[2] == 1) && (filt_name[0] == 0))
    {
        fprintf(stderr,
          " Error:  step 2 (locating pits) is reported as being complete,\n");
        fprintf(stderr,"  but elevation map layer name is not recorded.\n");
        printf("\nHit return to continue...");
        G_gets(buf);
        return;
    }

    G_get_window(&window);
    G_set_window(&window);

    V_clear();
    V_line(2,"PROJECT NAME:  ");
    V_const(proj_name,'s',2,17,40);

    V_line(4,step3);
    V_line(6,"Input:");
    V_line(7,"    Will use [filtered] elev data and pits information");
    V_line(8,"    Outlet coordinates:  easting:");
    V_line(9,"                        northing:");
    V_line(10,"    Pit threshold (meters):");
    V_line(13,"Output:");
    V_line(14,"    Drainage accumulation map layer:");
    V_line(15,"    Aspect map layer:");
    V_line(16,"      (adjusted pointer map)");
    V_line(17,"    Lakes map layer (optional):");
    V_line(19,"            Run accumulation program?");

/*  use "same units as elev data" above in place of "meters" if it is
    possible that elev data might be in something other than meters --
    check on this... */

    V_ques(&easting,'d',8,46,20);
    V_ques(&northing,'d',9,46,20);
    V_float_accuracy(4);
    V_ques(&pit_thres,'i',10,46,7);
    V_ques(river_name,'s',14,46,30);
    V_ques(aspect_name,'s',15,46,30);
    V_ques(lakes_name,'s',17,46,30);
    V_ques(run,'s',19,45,3);


    while (1)
    {
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (V_call() == 0)
        {
            easting = save_east;
            northing = save_north;
            pit_thres = save_pit;
            strcpy(river_name, save_river);
            strcpy(aspect_name, save_aspect);
            strcpy(lakes_name, save_lakes);

            return;
        }
    
        if ((river_name[0] == 0) || (aspect_name[0] == 0))
        {
            fprintf(stderr,"Please provide requested map layer names\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        if ((easting<window.west) || (northing>window.north)
          || (easting>window.east) || (northing<window.south))
        {
            printf("\nIllegal coordinates! -- please re-enter\n");
            printf("Elevation file window coordinates:\n");
            printf("  east:  %lf\n  west:  %lf\n", window.east, window.west);
            printf("  north:  %lf\n  south:  %lf\n", window.north, window.south);
            printf("Input coordinates:\n");
            printf("  Easting %lf, Northing %lf\n", easting, northing);

            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        if (pit_thres < 0)
        {
            fprintf(stderr,"Enter non-negative number for threshold\n");

            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        break;
    }

    if (strncmp(run,"y",1) == 0)
    {
        river_mapset = G_find_cell2(river_name,G_mapset());
        if (river_mapset != NULL)
        {
            printf("Drainage accumulation map layer already exists\n");
            printf("  ok to overwrite? [y/n]  ");
            answer = get_answer();

            if (answer == 'n')
            {
                river_mapset = G_ask_cell_new(
                  "Enter drainage accumulation map layer", river_name);
                if (!river_mapset)
                {
                    easting = save_east;
                    northing = save_north;
                    pit_thres = save_pit;
                    strcpy(river_name, save_river);
                    strcpy(aspect_name, save_aspect);
                    strcpy(lakes_name, save_lakes);

                    return;
                }
            }
        }

        aspect_mapset = G_find_cell2(aspect_name,G_mapset());
        if (aspect_mapset != NULL)
        {
            printf("Aspect map layer already exists\n");
            printf("  ok to overwrite? [y/n]  ");
            answer = get_answer();

            if (answer == 'n')
            {
                aspect_mapset = G_ask_cell_new("Enter aspect map layer",aspect_name);
                if (!aspect_mapset)
                {
                    easting = save_east;
                    northing = save_north;
                    pit_thres = save_pit;
                    strcpy(river_name, save_river);
                    strcpy(aspect_name, save_aspect);
                    strcpy(lakes_name, save_lakes);

                    return;
                }
            }
        }

        if (lakes_name[0] != 0)
        {
            lakes_mapset = G_find_cell2(lakes_name,G_mapset());
            if (lakes_mapset != NULL)
            {
                printf("Lakes map layer already exists\n");
                printf("  ok to overwrite? [y/n]  ");
                answer = get_answer();

                if (answer == 'n')
                {
                    lakes_mapset = G_ask_cell_new("Enter lakes map layer",
                      lakes_name);
                    if (!lakes_mapset)
                    {
                        easting = save_east;
                        northing = save_north;
                        pit_thres = save_pit;
                        strcpy(river_name, save_river);
                        strcpy(aspect_name, save_aspect);
                        strcpy(lakes_name, save_lakes);
    
                        return;
                    }
                }
            }

        }

        for (i=3; i<7; i++)
            complete[i] = 0;

        river_mapset = G_mapset();
        aspect_mapset = G_mapset();
        if (lakes_name[0] != 0)
            lakes_mapset = G_mapset();

        sprintf(command,
           "%s/etc/Griver -v 'elev=%s in %s' accum=%s drain=%s east=%lf north=%lf pthres=%d",
            G_gisbase(), filt_name, filt_mapset, river_name, aspect_name, 
            easting, northing, pit_thres);
        if (lakes_name[0] != 0)
        {
            strcat(command," lake=");
            strcat(command,lakes_name);
        }

        strcat(command,"\n");

        if (!system(command))
            complete[3] = 1;
        else
        {
            fprintf(stderr,"Bad exit status -- step not completed\n");
            printf("\nHit return to continue...");
            G_gets(buf);
        }
    }
    else
    {
        if ((save_east != easting) || (save_north != northing) ||
          (save_pit != pit_thres) || (strcmp(save_river, river_name)) || 
	  (strcmp(save_aspect, aspect_name)) || (strcmp(save_lakes, lakes_name)))
        {
            for (i=3; i<7; i++)
                complete[i] = 0;
        }
    }
}
