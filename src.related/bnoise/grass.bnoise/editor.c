#define EXTERN
#define PERM	2
#define CURRENT	3
#define LOCAL	5
#define ADD	1
#define DELETE	0

#include "gis.h"
#include "edit.h"

main(argc,argv) char *argv[];
{

    char *mapset;
    int option;
    char answer[5];
    char buf[100];
    char readbuf[1024];
    int errnum;
    int i,j;
    int addnum;
    int num_wtable;

    G_gisinit(argv[0]);

    G_get_window(&window);

    errnum = 0;

    if ((window.rows > 500) || (window.cols > 500))
        errnum++;

    if (((int)window.ew_res%50 != 0) || ((int)window.ns_res%50 != 0) ||
        (window.ew_res != window.ns_res))
        errnum++;

    if (errnum)
    {
        fprintf(stderr,"\nUnable to run program with current window\n");
        fprintf(stderr,"\nBnoise program has the following limitations:\n");
        fprintf(stderr,"    -window resolution must be a multiple of 50\n");
        fprintf(stderr,"      and must be the same for e-w and n-s\n");
        fprintf(stderr,"    -number of rows in window must be under 501\n");
        fprintf(stderr,"    -number of cols in window must be under 501\n");

        fprintf(stderr,"\nCurrent window does not meet these requirements\n");
        fprintf(stderr,"  please re-set and try again\n");

        printf("\nHit return to continue...");
        G_gets(buf);
        exit(-4);
    }

/*  window is changed here (temporarily) because the bnoise program
    produces result values on grid lines, but GRASS assigns values
    in between grid lines.  Hence, the artificial change of window
    here insures that the output from bnoise (on the new grid lines)
    will be for the centers of the old grids.  Window is changed
    back to that of current window (temporarily) each time the
    execution option is chosen by the user (in file run_Bnoise.c).
*/
    errnum = window.ns_res/2.;
    window.north -= errnum;
    window.south += errnum;
    window.east -= errnum;
    window.west += errnum;

    temp1_name = G_tempfile();

    num_weapons = 0;
    num_targets = 0;
    num_fp = 0;

    for (i=0; i<MAX_WEAP; i++)
        weap_codes[i] = 1;

    sprintf(perm_name,"%s/etc/gun.table",G_gisbase());
    perm_fd = fopen(perm_name,"r");
/*    perm_fd = fopen("new.table","r"); */
    if (!perm_fd)
    {
        fprintf(stderr,"Error in opening main gun table file\n");
        exit(2);
    }
    fclose(perm_fd);

    num_targets = 0;

    for (i=0; i<MAX_TARG; i++)
    {
        targets[i].id[0] = 0;
        targets[i].easting = 0.;
        targets[i].northing = 0.;
        targets[i].gc_fact = 0.;
    }


    V_clear();
    V_line(3, "            WELCOME TO THE BNOISE INTERFACE WITH GRASS");
    V_line(5, "       Within this interface, you will be prompted for information");
    V_line(6, "       necessary for the running of the bnoise program.  The");
    V_line(7, "       output from this is currently restricted to the data needed");
    V_line(8, "       for the creation of a GRASS raster file.  This map layer");
    V_line(9, "       will depict the noise contours resulting from the given");
    V_line(10, "       scenario of target points, firing points, and weapons.");
    V_line(12, "       Default values (if available) are given in brackets: []");
    V_line(14, "       Your current GRASS location, mapset and window are used.");

    V_intrpt_ok();
    if (V_call() == 0)
        exit(2);

    while (1)
    {
        option = 0;
        V_clear();
        V_line(1, "WEAPONS");
        V_line(2, " 1   Choose weapon(s) from location specific table");
        V_line(3, " 2   Choose weapon(s) from general table");
        V_line(4, " 3   Add weapon(s)");
        V_line(5, " 4   Delete weapon(s) from current list");
        V_line(6, " 5   Review chosen weapons");

        V_line(7, "TARGET POINTS");
        V_line(8, " 6   Choose target(s) from location specific file");
        V_line(9, " 7   Add target point(s)");
        V_line(10," 8   Delete target point(s) from current list");
        V_line(11," 9   Review chosen target point(s)");

        V_line(12,"FIRING POINTS");
        V_line(13," 10  Choose firing point(s) from location specific file");
        V_line(14," 11  Add firing point(s)");
        V_line(15," 12  Delete firing point(s) from current list");
        V_line(16," 13  Review chosen firing point(s)");

        V_line(18," 14  Save current parameters");
        V_line(19," 15  Remove parameter storing files");
        V_line(20," 16  Execute bnoise");

        V_line(21," 17  Quit                  Choose option: ");
        V_ques(&option,'i',21,44,3);

        V_call();
/*
        while (1)
        {
            V_intrpt_ok();
            if (!V_call())
                finish();
            else
                break;
        }
*/
        switch (option)
        {
            case 1:    mapset = G_find_file("noise","weapons","");
                       if (!mapset)
                       {
                           fprintf(stderr,"Error -- no weapons files available\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       gun_mapset = G_ask_old("Enter gun table file",
                           gun_name,"noise/weapons", "weapons storing");
                       if (!gun_mapset)
                           break;

                       gun_fd = G_fopen_old("noise/weapons", gun_name, gun_mapset);
                       if (!gun_fd)
                       {
                           fprintf(stderr,"Error in opening existing gun table file\n");
                           break;
                       }

                       if (sscanf(fgets(readbuf,1024,gun_fd), "%d", &num_wtable) != 1)
                       {
                           fprintf(stderr,"Error reading location specific file\n");
                           exit(7);
                       }

                       if (num_wtable > MAX_WEAP)
                       {
                           fprintf(stderr,
                             "Error from file -- not allowed more than %d weapon types\n",
                             MAX_WEAP);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_wtable,sizeof(WEAPONS));
                       read_weapon(gun_fd, 0, num_wtable, LOCAL);
                       fclose (gun_fd);

                       choose_weapon(num_wtable,ADD);
                       free(weapons);
                       break;

            case 2:    perm_fd = fopen(perm_name,"r");
/*                       perm_fd = fopen("new.table","r"); */
                       if (!perm_fd)
                       {
                           fprintf(stderr,"Error in opening main gun table file\n");
                           break;
                       }

                       if (sscanf(fgets(readbuf,1024,perm_fd), "%d", &num_wtable) != 1)
                       {
                           fprintf(stderr,"Error reading permanent table\n");
                           exit(7);
                       }

                       if (num_wtable > MAX_WEAP)
                       {
                           fprintf(stderr,
                             "Error from file -- not allowed more than %d weapon types\n",
                             MAX_WEAP);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_wtable,sizeof(WEAPONS));
                       read_weapon(perm_fd, 0, num_wtable, PERM);
                       fclose(perm_fd);

                       choose_weapon(num_wtable,ADD);
                       free(weapons);
                       break;

            case 3:    add_weapon();
                       break;

            case 4:    if (num_weapons == 0)
                       {
                           fprintf(stderr,"Impossible to delete -- number of chosen weapons is 0\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
                       temp1_fd = fopen(temp1_name,"r");
                       read_weapon(temp1_fd,0,num_weapons,-3);
                       fclose(temp1_fd);

                       num_wtable = num_weapons;
                       num_weapons = 0;

                       choose_weapon(num_wtable,DELETE);
                       free(weapons);
                       break;

            case 5:    if (num_weapons == 0)
                       {
                           fprintf(stderr,"Impossible to review -- number of chosen weapons is 0\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
                       temp1_fd = fopen(temp1_name,"r");
                       read_weapon(temp1_fd,0,num_weapons,0);
                       fclose(temp1_fd);

                       show_weapons();
                       free(weapons);
                       break;

            case 6:    mapset = G_find_file("noise","targets","");
                       if (!mapset)
                       {
                           fprintf(stderr,"Error -- no target files available\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       targ_mapset = G_ask_old("Enter target file",
                           targ_name,"noise/targets", "target ");
                       if (!targ_mapset)
                           break;

                       targ_fd = G_fopen_old("noise/targets", targ_name, targ_mapset);
                       if (!targ_fd)
                       {
                           fprintf(stderr,"Error in opening existing target file\n");
                           break;
                       }

                       if (sscanf(fgets(readbuf,1024,targ_fd), "%d", &num_wtable) != 1)
                       {
                           fprintf(stderr,"Error reading location specific file\n");
                           exit(7);
                       }

                       if (num_wtable > MAX_TARG)
                       {
                           fprintf(stderr,
                             "Error from file -- not allowed more than %d target points\n",
                             MAX_TARG);
                           break;
                       }

                       read_target(targ_fd, num_wtable);
                       fclose (targ_fd);

                       choose_target(num_wtable,ADD);
                       break;

            case 7:    add_target();
                       break;

            case 8:    if (num_targets == 0)
                       {
                           fprintf(stderr,"Impossible to delete -- number of chosen targets is 0\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;

                       }

                       for (i=0; i<num_targets; i++)
                       {
                           strcpy(temp_targ[i].id,targets[i].id);
                           temp_targ[i].easting = targets[i].easting;
                           temp_targ[i].northing = targets[i].northing;
                           temp_targ[i].gc_fact = targets[i].gc_fact;
                           targets[i].id[0] = 0;
                           targets[i].easting = 0.;
                           targets[i].northing = 0.;
                           targets[i].gc_fact = 0.;
                       }

                       num_wtable = num_targets;
                       num_targets = 0;

                       choose_target(num_wtable,DELETE);
                       break;

            case 9:    if (num_targets == 0)
                       {
                           fprintf(stderr,"Impossible to review -- number of chosen targets is 0\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       show_targets();
                       break;

            case 10:   if ((num_weapons == 0) || (num_targets == 0))
                       {
                           fprintf(stderr,
                             "Must enter or choose weapon types and targets first\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
                       temp1_fd = fopen(temp1_name,"r");
                       read_weapon(temp1_fd,0,num_weapons,0);
                       fclose(temp1_fd);

                       mapset = G_find_file("noise","firings","");
                       if (!mapset)
                       {
                           fprintf(stderr,"Error -- no firing point files available\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       fp_mapset = G_ask_old("Enter firing point file",
                           fp_name,"noise/firings", "firing point");
                       if (!fp_mapset)
                           break;

                       fp_fd = G_fopen_old("noise/firings", fp_name, fp_mapset);
                       if (!fp_fd)
                       {
                           fprintf(stderr,"Error in opening existing firing points file\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       if (sscanf(fgets(readbuf,1024,fp_fd), "%d", &num_wtable) != 1)
                       {
                           fprintf(stderr,"Error reading location specific file\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       if (num_wtable > MAX_FP)
                       {
                           fprintf(stderr,
                             "Error from file -- not allowed more than %d firing points\n",
                             MAX_FP);
                           break;
                       }

                       temp_fp =
                         (FIRPOINT *)G_calloc(num_wtable,sizeof(FIRPOINT));
                       read_fp(fp_fd, num_wtable);
                       fclose (fp_fd);

                       choose_fp(num_wtable,ADD);

                       free(weapons);
                       free(temp_fp);

                       break;

            case 11:   if ((num_weapons == 0) || (num_targets == 0))
                       {
                           fprintf(stderr,
                             "Must enter or choose weapon types and targets first\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
                       temp1_fd = fopen(temp1_name,"r");
                       read_weapon(temp1_fd,0,num_weapons,0);
                       fclose(temp1_fd);

                       add_fp();
                       free(weapons);

                       break;

            case 12:   if (num_fp == 0)
                       {
                           fprintf(stderr,"Impossible to delete -- number of chosen firing points is 0\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;

                       }

                       temp_fp = (FIRPOINT *)G_calloc(num_fp,sizeof(FIRPOINT));
                       for (i=0; i<num_fp; i++)
                       {

                           strcpy(temp_fp[i].id, firing[i].id);
                           temp_fp[i].easting = firing[i].easting;
                           temp_fp[i].northing = firing[i].northing;
                           temp_fp[i].gc_fact = firing[i].gc_fact;
                           temp_fp[i].num_weap = firing[i].num_weap;
                           temp_fp[i].ptr = (FP_INFO *)G_calloc(firing[i].num_weap,
                             sizeof(FP_INFO));

                           for(j=0; j<temp_fp[i].num_weap; j++)
                           {
                               temp_fp[i].ptr[j].weap_code = firing[i].ptr[j].weap_code;
                               temp_fp[i].ptr[j].num_day = firing[i].ptr[j].num_day;
                               temp_fp[i].ptr[j].num_night = firing[i].ptr[j].num_night;
                               temp_fp[i].ptr[j].min_charge = firing[i].ptr[j].min_charge;
                               temp_fp[i].ptr[j].max_charge = firing[i].ptr[j].max_charge;
                               strcpy(temp_fp[i].ptr[j].targ_id, firing[i].ptr[j].targ_id);
                               strcpy(temp_fp[i].ptr[j].noise, firing[i].ptr[j].noise);
                               temp_fp[i].ptr[j].height = firing[i].ptr[j].height;
                           }
                       }

                       num_wtable = num_fp;
                       num_fp = 0;
                       free(firing);

                       choose_fp(num_wtable,DELETE);
                       free(temp_fp);
                       break;

            case 13:   if (num_fp == 0)
                       {
                           fprintf(stderr,"Impossible to review -- number of chosen firing points is 0\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       show_fp();
                       break;

            case 14:   save_param();
                       break;

            case 15:   remove_param();
                       break;

            case 16:   if ((num_weapons == 0) || (num_targets == 0) ||
                           (num_fp == 0))
                       {
                           fprintf(stderr,
                             "Error -- must have chosen weapons, targets and firing points before execution is possible\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       if (num_targets == 0)
                       {
                           fprintf(stderr,
                             "Must enter or choose target points first\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       if (num_fp == 0)
                       {
                           fprintf(stderr,
                             "Must enter or choose firing points first\n");

                           printf("\n Hit return to continue...");
                           G_gets(buf);
                           break;
                       }

                       weapons = (WEAPONS *)G_calloc(num_weapons,sizeof(WEAPONS));
                       temp1_fd = fopen(temp1_name,"r");
                       read_weapon(temp1_fd,0,num_weapons,0);
                       fclose(temp1_fd);

                       run_Bnoise();
                       free(weapons);
                       break;

            case 17:   finish();
                       break;

            default:   fprintf(stderr,"Enter number between 1 and 17\n");
                       sleep(3);
                       break;
        }
    }

}
