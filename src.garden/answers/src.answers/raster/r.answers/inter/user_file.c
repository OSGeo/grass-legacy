/* %W% %G% */

#include "gis.h"

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

   function:  user_file
   called by: answers_run, get_datafile_name, mk_chnl_tbl, mk_cover_tbl
   mk_rain, mk_soil_tbl, show_status 

   allows user to make a copy of a data file for their
   own perusal
   */
user_file(tmpname)
    char *tmpname;
{
    char ans[256], command[256], name[128];
    char printer[256];
    int option;
    int line;

    while (1)
    {
	option = 0;
        line = 7;

	V_clear();
	V_line(line++, "           ANSWERS on GRASS file handler");
        line++;
	V_line(line++, "                Choose desired option:");
        line++;
	V_line(line++, "                  0.  Exit this function");
	V_line(line++, "                  1.  Look at file now");
	V_line(line++, "                  2.  Copy to a file");
	V_line(line++, "                  3.  Send to printer");
        line++;
	V_line(line, "          Option:  ");

	V_ques(&option, 'i', line, 18, 2);


	V_intrpt_msg("EXIT");
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
            sprintf(command, "clear; more %s", tmpname);
            G_system(command);
            hit_return();
	    continue;

          /*----*/
	  case 2: 
    while (1)
    {
	    fprintf(stderr,"\nEnter the file name --> ");
	    if (!G_gets(ans))
		continue;
	    if(sscanf(ans,"%s",name) != 1)
		continue;
	    G_strip (name);
	    if (*name == 0)
		continue;
	    if (access (name, 0) == 0)
	    {
	        sprintf (ans, "%s exists. Ok to overwrite? ", name);
	        if (!G_yes(ans,-1))
                    continue;
	    }
/*
	    if(name[0] != '/')
	    {
		sprintf(command,"cp %s %s/%s",tmpname,G_home(),name);
		fprintf(stderr,"'%s' being saved in your home directory",name);
	    }
	    else
*/
	    {
		sprintf(command,"cp %s %s",tmpname,name);
		fprintf(stderr,"'%s' being saved",name);
	    }
	    system(command);
	    fprintf(stderr,"\n");
            sleep(3);
	break;
    }
	    continue;

	  /*---*/
          case 3:  
            G_clear_screen();
            printf("\n");
            printf("\n");
            strcpy(printer, "lpr");
            if ((getenv("PRINTER")) != NULL)
            {
                strcat(printer, " -P");
                strcat(printer, getenv("PRINTER"));
            }

	    fprintf(stderr, "Enter the printer command [ %s ] > ", printer);
	    while (!G_gets(name));
	        G_strip (name);
	    if (*name == 0) strcpy (name, printer);
            sprintf(command, "%s %s", name, tmpname);
            fprintf(stderr, "\nexecuting print command...\n\n\n");
            G_system(command);
            fprintf(stderr,"\n");
            hit_return();
            continue;

	  /*---*/
          case 4:  
            sprintf(command, "vi %s", tmpname);
            G_system(command);
	    continue;

          /*----*/
	  default:
	    fprintf(stderr, "Please choose a given option number\n");
	    sleep(2);
	    break;
	}
	if ((option >= 0) && (option < 5))
	    break;
    }
    return(0);
  }

