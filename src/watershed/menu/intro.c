#define EXTERN

#include "gis.h"
#include "water.h"


main(argc,argv) char *argv[];
{

    int i;
    FILE *proj_fd;
    int option;
    int error;

    G_gisinit(argv[0]);

/*  initialize all variables used as character strings, and the table
    for tallying which programs in this series have been completed  */

    for (i=0; i<7; i++)
        complete[i] = 0;

    proj_name[0] = 0;

    elev_name[0] = 0;
    filt_name[0] = 0;
    filt_num = 0;

    river_name[0] = 0;
    aspect_name[0] = 0;
    lakes_name[0] = 0;

    thin_name[0] = 0;

    extthin_name[0] = 0;
    clthin_name[0] = 0;

    basin_name[0] = 0;

    strcpy(step1,"Step1:  Filtering of elevation data");
    strcpy(step2,"Step2:  Locating pits");
    strcpy(step3,"Step3:  Drainage accumulation/watershed identification");
    strcpy(step4,"Step4:  Thinning of accumulation file");
    strcpy(step5,"Step5:  Channel segment coding");
    strcpy(step6,"Step6:  Subwatershed designation");

/*  First screen, introducing the system, and asking for a project title.
    The project under this title may have already begun.  If so, the
    project information will be searched for and read in from the file.  */

    while (1)
    {

        error = 0;
        option = 0;

        V_clear();
        V_line(1, "         WELCOME TO THE WATERSHED SOFTWARE SYSTEM");
        V_line(3, "  This program is designed to help you follow the correct");
        V_line(4, "  steps to obtain a GRASS raster file depicting watershed");
        V_line(5, "  geometry using a raster elevation file (with true elevation");
        V_line(6, "  values) as a starting point.  All steps are recorded");
        V_line(7, "  under a project name.");

        V_line(10,"  Choose desired option:");
        V_line(11,"  1.  Create new project");
        V_line(12,"  2.  Create new project based on currently existing project");
        V_line(13,"  3.  Work on an existing project");
        V_line(16,"  4.  Remove old files");

        V_line(19,"       Desired option:  ");

        V_ques(&option,'i',19,25,2);


        V_intrpt_msg("EXIT");
        V_intrpt_ok();
        if (V_call() == 0)
            exit(2);

        switch (option)
        {
        case 1:    proj_mapset = G_ask_new("Enter project name:",proj_name,
                     "watershed/project", "parameter storing","");
                   if (!proj_mapset)
                   {
                       error = 1;
                       break;
                   }
                   proj_fd = G_fopen_new("watershed/project", proj_name);
                   if (!proj_fd)
                   {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                         proj_name);
                           error = 1;
                           break;
                   }
                   write_title(proj_fd);
                   fclose(proj_fd);
                   break;

        case 2:    proj_mapset = G_ask_old("Enter old project name:",proj_name,
                     "watershed/project", "parameter storing","");
                   if (!proj_mapset)
                   {
                       error = 1;
                       break;
                   }
                   proj_fd = G_fopen_old("watershed/project", proj_name,
                     proj_mapset);
                   if (!proj_fd)
                   {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                         proj_name);
                       error = 1;
                       break;
                   }
                   read_title(proj_fd);
                   fclose(proj_fd);
                   proj_mapset = G_ask_new("Enter new project name:",proj_name,
                     "watershed/project", "parameter storing","");
                   if (!proj_mapset)
                   {
                       error = 1;
                       break;
                   }
                   proj_fd = G_fopen_new("watershed/project", proj_name);
                   if (!proj_fd)
                   {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                         proj_name);
                       error = 1;
                       break;
                   }
                   write_title(proj_fd);
                   fclose(proj_fd);
                   break;

        case 3:    proj_mapset = G_ask_old("Enter existing project name:",proj_name,
                     "watershed/project", "parameter storing","");
                   if (!proj_mapset)
                   {
                       error = 1;
                       break;
                   }
                   proj_fd = G_fopen_old("watershed/project", proj_name,
                     proj_mapset);
                   if (!proj_fd)
                   {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                         proj_name);
                       error = 1;
                       break;
                   }
                   read_title(proj_fd);
                   fclose(proj_fd);
                   break;

        case 4:    remove_files();
                   break;

        default:   fprintf(stderr,"Please choose a given option number\n");
                   sleep(2);
                   break;
        }

        if (( option > 0 ) && ( option < 4 ) && (!error))
            break;

    }

    while (1)
    {
        option = 0;

        V_clear();
        V_line(2,"PROJECT NAME:  ");
        V_const(proj_name,'s',2,17,40);

        V_line(4,"Choose desired option:");
        if (!complete[1])
            V_line(6, "1.  Filtering of elevation data");
        else
            V_line(6,
            "1.  Filtering of elevation data                            COMPLETE");
        if (!complete[2])
            V_line(7, "2.  Locating pits");
        else
            V_line(7,
            "2.  Locating pits                                          COMPLETE");
        if (!complete[3])
            V_line(8, "3.  Calculating drainage accumulation/outlining watershed");
        else
            V_line(8,
            "3.  Calculating drainage accumulation/outlining watershed  COMPLETE");
        if (!complete[4])
            V_line(9, "4.  Creating stream network");
        else
            V_line(9,
            "4.  Creating stream network                                COMPLETE");
        if (!complete[5])
            V_line(10,"5.  Coding stream segments/finding segment lengths");
        else
            V_line(10,
            "5.  Coding stream segments/finding segment lengths         COMPLETE");
        if (!complete[6])
            V_line(11,"6.  Finding subwatershed basins");
        else
            V_line(11,
            "6.  Finding subwatershed basins                            COMPLETE");
        V_line(13,"7.  Quit");
    
        V_line(18,"    Choose desired option:  ");
    
        V_ques(&option,'i',18,29,3);

        V_call();

        switch (option)
        {
        case 1:    filt_step();
                   break;

        case 2:    pits_step();
                   break;

        case 3:    riv_step();
                   break;

        case 4:    thin_step();
                   break;

        case 5:    code_step();
                   break;

        case 6:    basin_step();
                   break;

        case 7:    proj_fd = G_fopen_new("watershed/project",
                       proj_name, proj_mapset);
                   if (!proj_fd)
                   {
                       fprintf(stderr,"Problem opening project file [%s]\n",
                         proj_name);
                       exit(-2);
                   }
                   write_title(proj_fd);
                   fclose(proj_fd);
                   exit(0);
                   break;

        default:   fprintf(stderr,"Enter number between 1 and 7\n");
                   sleep(2);
                   break;
        }
    }


}
