#define EXTERN

#include "menu.h"


main(argc,argv) char *argv[];
{

    char answer;
    int i;
    char get_answer();
    FILE *proj_fd;
    int option;
    char opt[2][2];
    char buf[80];

    G_gisinit(argv[0]);

    elev_name[0] = 0;
    extthin_name[0] = 0;
    basin_name[0] = 0;

    part_name[0] = 0;
    soils_name[0] = 0;
    cover_name[0] = 0;

    for (i=0; i<2; i++)
        opt[i][0] = 0;

    V_clear();

    V_line(2,"GRASS.ARMSED");

    V_line(4," This program provides a menu for completing the interface");
    V_line(5," to ARMSED.  If you have not yet finished the series of steps");
    V_line(6," provided in program 'watershed', exit now and do so. ");

    V_line(8," If you have completed the 'watershed' series, please");
    V_line(9," denote your choice below.");

    V_line(12,"  CHOOSE DESIRED OPTION");

    V_line(14," ___  Start new armsed project using 'watershed' products");
    V_line(15," ___  Continue an existing armsed project");

    V_ques(opt[0],'s',14,2,1);
    V_ques(opt[1],'s',15,2,1);

    while (1)
    {
        V_intrpt_msg("EXIT");
        V_intrpt_ok();

        if (!V_call())
            exit(2);

        i = 0;

        if (!strncmp(opt[0],"x",1))
            i++;
        if (!strncmp(opt[1],"x",1))
            i++;

        if (i != 1)
        {
            fprintf(stderr,"Choose one and only one option\n");
            printf("\nHit return to continue...");
            G_gets(buf);
            continue;
        }

        if (strncmp(opt[0],"x",1) == 0)
        {
            proj_mapset = G_ask_old("Enter project name", proj_name,
              "watershed/project", "parameter storing");
            if (!proj_mapset)
                continue;
        }

        if (strncmp(opt[1],"x",1) == 0)
        {
            proj_mapset = G_ask_old("Enter project name", proj_name,
              "multsed/project", "parameter storing");
            if (!proj_mapset)
                continue;
        }
        break;
    }

    if (strncmp(opt[0],"x",1) == 0)
    {

        proj_fd = G_fopen_old("watershed/project", proj_name,
          proj_mapset);
        if (!proj_fd)
        {
            fprintf(stderr,"Problem opening project file [%s]\n",
              proj_name);
            exit(-2);
        }

        get_proj(proj_fd);
        fclose(proj_fd);
    }

    if (strncmp(opt[1],"x",1) == 0)
    {

        proj_fd = G_fopen_old("multsed/project", proj_name,
          proj_mapset);
        if (!proj_fd)
        {
            fprintf(stderr,"Problem opening project file [%s]\n",
              proj_name);
            exit(-2);
        }

        read_mult(proj_fd);
        fclose(proj_fd);
    }

    while (1)
    {
        option = 0;

        V_clear();
        V_line(2,"PROJECT NAME:  ");
        V_const(proj_name,'s',2,17,40);

        V_line(6,"Choose desired option:");

        V_line(8, "1.  Partition into left/right half planes");
        V_line(9, "2.  Collect statistics for armsed");
        V_line(10, "3.  Provide remaining armsed input");
        V_line(11, "4.  Run armsed");

        V_line(13,"5.  Quit");
    
        V_line(16,"    Desired option number:  ");
    
        V_ques(&option,'i',16,29,3);

        V_intrpt_msg("QUIT");
        V_intrpt_ok();

        if (!V_call())
            finish();

        switch (option)
        {
        case 1:    partition_step();
                   break;

        case 2:    stat_step();
                   break;

        case 3:    input_step();
                   break;

        case 4:    run_armsed();
                   break;

        case 5:    finish();
                   break;

        default:   fprintf(stderr,"Enter number between 1 and 5\n");
                   sleep(3);
                   break;
        }
    }

}
