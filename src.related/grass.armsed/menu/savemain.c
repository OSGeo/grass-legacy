#define EXTERN

#include "menu.h"


main(argc,argv) char *argv[];
{

    char answer;
    int i;
    char get_answer();
    FILE *proj_fd;
    int option;

    G_gisinit(argv[0]);

    for (i=0; i<6; i++)
        complete[i] = 0;

    extthin_name[0] = 0;
    basin_name[0] = 0;

    part_name[0] = 0;
    soils_name[0] = 0;
    cover_name[0] = 0;

    V_clear();

    V_line(2,"GRASS.ARMSED");

    V_line(4," This program provides a menu for completing the interface");
    V_line(5," to ARMSED.  If you have not yet finished the series of steps");
    V_line(6," provided in program 'watershed', exit now and do so. ");

    V_line(8," If you have completed the 'watershed' series, please",
    V_line(9," denote your choice below.");

    V_line(12,"  CHOOSE DESIRED OPTION");

    V_line(14," ___  Start new armsed project using 'watershed' products");
    V_line(15," ___  Continue an existing armsed project");

    V_ques(
"the watershed series.",
    for (i=0; intro[i]; i++)
	printf("%s\n", intro[i]);

    proj_mapset = G_ask_old("Enter project name", proj_name,
      watershed/project, "parameter storing");
    if (!proj_mapset)
        exit(-2);

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

    while (1)
    {
        option = 0;

        V_clear();
        V_line(2,"PROJECT NAME:  ");
        V_const(proj_name,'s',2,17,40);

        V_line(4,"Choose desired option:");
        if (!complete[1])
            V_line(6, "1.  Partition into left/right half planes");
        else
            V_line(6,
	    "1.  Partition into left/right half planes    COMPLETE");

        if (!complete[2])
            V_line(7, "2.  Collect statistics for armsed");
        else
            V_line(7,
	    "2.  Collect statistics for armsed            COMPLETE");

        if (!complete[3])
            V_line(8, "3.  Provide remaining armsed input");
        else
            V_line(8,
	    "3.  Provide remaining armsed input           COMPLETE");

        if (!complete[4])
            V_line(9, "4.  Run armsed");
        else
            V_line(9,
	    "4.  Run armsed                               COMPLETE");

        V_line(14,"5.  Quit");
    
        V_line(16,"    Choose desired option:  ");
    
        V_ques(&option,'i',16,29,3);

        V_call();

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

        default:   fprintf(stderr,"Enter number between 1 and 6\n");
                   sleep(3);
                   break;
        }
    }

}
