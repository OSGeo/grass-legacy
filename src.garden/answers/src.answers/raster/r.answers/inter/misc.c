/* %G% %W% */
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

   function: misc
   called by: menu

   allow user access to files in the project database, so they can
   view, copy, print, and perhaps edit. 

   */

#include "answers.h"

misc()
{
    int option;
    int line;

    while (1)
    {
	option = 0;
        line = 1;

	V_clear();
	V_line(line++, "           ANSWERS on GRASS Miscellaneous Functions");
        line++;
        V_line(line++,
        "This function allows access to files in the project database.");
        V_line(line++,
        "These files are created for each project for use by the programs.");
        V_line(line++,
        "This function can also be used to produce a report of the current");
        V_line(line++,
        "project status.");
        line++;
	V_line(line++, "                   Choose desired option:");
        line++;
	V_line(line++, "                  0.  Exit to Main Menu");
	V_line(line++, "                  1.  Access database files");
	V_line(line++, "                  2.  Show project status");
        line++;
	V_line(line, "          Option:  ");

	V_ques(&option, 'i', line, 18, 2);


	V_intrpt_msg("EXIT to Main Menu");
	V_intrpt_ok();
	if (V_call() == 0)
	    return(0);

	switch (option)
	{
	  /*---*/
          case 0:  
	    return(0);

	  /*---*/
	  case 1: 
            list_files();
	    continue;

          /*----*/
	  case 2: 
            show_status();
	    continue;

          /*----*/
	  default:
	    fprintf(stderr, "Please choose a given option number\n");
	    sleep(2);
	    break;
	}
	if ((option >= 0) && (option < 3))
	    break;
    }
    return(0);
  }

/*------------------------------*/
list_files()
{
    char command[1024];

    G_clear_screen();
    fprintf(stderr, "\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "ANSWERS on GRASS data files\n");
    fprintf(stderr, "-----------------------------------------------------\n");
    
    sprintf(command, "ls %s/%s/%s", G_location_path(), G_mapset(), data_dir);
    G_system(command);
    fprintf(stderr, "-----------------------------------------------------\n");
    fprintf(stderr, "rain gauge data files\n");
    fprintf(stderr, "-----------------------------------------------------\n");
    sprintf(command, "ls %s/%s/%s/rain", G_location_path(), G_mapset(), data_dir);
    G_system(command);
    fprintf(stderr, "-----------------------------------------------------\n\n");

    if( G_yes("Do you want to read/copy/print a file?", 1))
    {
        if(G_yes("Look at a rain data file", 0))
            get_datafile_name(1);
        else
            get_datafile_name(0);
    }
    return(0);

}
/*--------------------------------
  get the name of file in database, set the external variable 
  file_name to this. then we can use more/lpr/cp/vi with this
  name as the user wishes. since rain files are one dir beneath
  the main dir of data files, we use the type flag to let us know
  to add "rain" to the path to the file 
----------------------------------*/
get_datafile_name(type)
    int type;

{

    char name[256];
    char dir[256];
    char file_name[256];

    strcpy(dir, data_dir);

    if (type == 1)
        strcat(dir, "/rain");

    *file_name = '\0';
    if(!G_ask_old("Enter datafile name:\n", name, dir,
    "ANSWERS on GRASS data"))
        return(0);
    sprintf(file_name, "%s/%s/%s/%s", G_location_path(), G_mapset(),
    dir, name);
    user_file(file_name);
/*
    fprintf(stderr, "\nname: %s\n", file_name);
*/
    return(0);
}
