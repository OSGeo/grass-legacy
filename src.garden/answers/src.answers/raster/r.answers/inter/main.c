/* %W% %G% */
/* 
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. Permission to use, copy,    |
    |   modify, and distribute this software and its          |
    |   documentation for any purpose and without fee is      |
    |   hereby granted, provided that the above copyright     |
    |   notice appear in all copies.  This software is        |
    |   provided "as is" without express or implied warranty. |
    +---------------------------------------------------------+

   function:  main
   called by: user

   This is main for the ANSWERS on GRASS project manager.
   A answers/project directory is created and/or used to store
   a file 'project' to record the user's progress. Other files
   are also created by funtions as needed in this directory. 
   */

#include "version.h"
#include "answers.h"

main(argc, argv)
char *argv[];
{

    int i;
    int option;
    int error;
    char line[80];

    G_gisinit(argv[0]);

/*  initialize all variables used as character strings, and the table
    for tallying which programs in this series have been completed  */

    for (i = 0; i <= 14; i++)
	complete[i] = 0;

    proj_mapset=G_mapset();

/* copy the version number string in version.h to the global var */

    strcpy(version_num, version);

/* need one of these for each step to serve as menu title */
    i = 1;
    strcpy(step[i++], "Set mask, region, and resolution");
    strcpy(step[i++], "Catalogue soils parameters");
    strcpy(step[i++], "Catalogue land use and surface parameters");
    strcpy(step[i++], "Identify elevation-based input layers");
    strcpy(step[i++], "Prepare rain gauge data");
    strcpy(step[i++], "Identify outlet cell");
    strcpy(step[i++], "Specify areas with subsurface drainage");
    strcpy(step[i++], "Catalogue channel parameters");
    strcpy(step[i++], "Define channel slopes");
    strcpy(step[i++], "Specify BMP's in watershed");
    strcpy(step[i++], "Prepare ANSWERS input and run simulation");

/* set bmp titles */

    strcpy(bmp_tbl[0].title, "Tile Outlet Terrace");
    strcpy(bmp_tbl[1].title, "Sedimentation Pond");
    strcpy(bmp_tbl[2].title, "Grassed Waterway");
    strcpy(bmp_tbl[3].title, "Field Border");

    if (argc > 1)
    {
	strcpy(proj_name, argv[1]);
        sprintf( data_dir, "answers/data/%s", proj_name);
        read_project();
        set_mask();
        out_fp = stderr;
        set_region();
        /*-------- X ---------
        complete[11]=2;
        answers_input();
        exit(0);
        -------- X ---------*/
        menu();
        exit(0);
     }

/*  First screen, introducing the system, and asking for a project title.
    The project under this title may have already begun.  If so, the
    project information will be searched for and read in from the file.  */

    sprintf(line, "                        version %s", version_num);

    while (1)
    {

	error = 0;
	option = 0;

	V_clear();
	V_line(1, "              ANSWERS on GRASS Project Manager");
        V_line(2, line);
	V_line(4, "  This program is designed to help you use information in");
	V_line(5, "  GRASS raster layers to create an input file to run on a");
	V_line(6, "  standard version of the ANSWERS watershed simulation program.");
	V_line(7, "  All steps of this process are recorded in data files");
	V_line(8, "  stored under a project name in your GRASS database.");

	V_line(11, "                   Choose desired option:");
	V_line(13, "                  0.  Exit");
	V_line(14, "                  1.  Create new project");
	V_line(15, "                  2.  Work on an existing project");
	V_line(16, "                  3.  Copy an existing project");
	V_line(17, "                  4.  Remove an existing project");

	V_line(19, "          Option:  ");

	V_ques(&option, 'i', 19, 18, 2);


	V_intrpt_msg("EXIT");
	V_intrpt_ok();
	if (V_call() == 0)
	    exit(2);

	switch (option)
	{
	  /*---*/
          case 0:  
	    exit(0);
	    break;

	  /*---*/
	  case 1: 
	    proj_mapset = G_ask_new("Enter project name:\n", proj_name,
		"answers/project", "ANSWERS on GRASS project");
	    if (!proj_mapset)
	    {
		error = 1;
		break;
	    }
            sprintf( data_dir, "answers/data/%s", proj_name);
	    if (step_1() == 1)
	    {
	        error = 1;
	        break;
	    }
	    save_project();
	    break;

          /*----*/
	  case 2: 
	    proj_mapset = G_ask_in_mapset("Enter existing project name:",
            proj_name, "answers/project", "ANSWERS on GRASS project");
	    if (!proj_mapset)
	    {
		error = 1;
		break;
	    }
            sprintf( data_dir, "answers/data/%s", proj_name);
	    if (read_project() == 1)
            {
                error = 1;
                break;
            }
	    if (set_mask()==1)
	    {
	        fprintf(stderr, "\n\7WARNING: Could not set project mask.\n");
	        hit_return();
	        error = 1;
	        break;
	    }
	    if (set_region()==1)
	    {
	        fprintf(stderr, "\n\7WARNING: Could not set project region.\n");
	        hit_return();
	        error = 1;
	        break;
	    }
	    /*
	    show_status();
	    */
	    break;

          /*----*/
	  case 3:
	    copy_project();
	    continue;

          /*----*/
	  case 4:
	    remove_files();
	    continue;

          /*----*/
	  default:
	    fprintf(stderr, "Please choose a given option number\n");
	    sleep(2);
	    break;
	}

	if ((option >= 0) && (option < 5) && (!error))
	    break;

    }
    menu();
  }
