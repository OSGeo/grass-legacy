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

   function:  menu
   called by: main

   This is the main menu for the ANSWERS on GRASS project manager.
 
   */

#include "answers.h"

menu()
{
    int option, i;
    char line[15][90];
    extern int hit_return();
    char *status();

    while (1)
    {
        option = 0;

        V_clear();
        V_line(1,"   ANSWERS on GRASS Project Manager Main Menu");
        V_line(2,"   Project Name:  ");
        V_const(proj_name,'s',2,18,40);

/* create menu from steps array */
V_line(4," Status Option Description");
V_line(5,"---------------------------------------------------------");

  i = 0;
  sprintf(line[0], "          %d    Quit ", i);
  V_line(i+6, line[0]);
  i++;
  sprintf(line[1], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(7, line[1]);
  i++;
  sprintf(line[2], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(8, line[2]);
  i++;
  sprintf(line[3], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(9, line[3]);
  i++;
  sprintf(line[4], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(10, line[4]);
  i++;
  sprintf(line[5], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(11, line[5]);
  i++;
  sprintf(line[6], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(12, line[6]);
  i++;
  sprintf(line[7], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(13, line[7]);
  i++;
  sprintf(line[8], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(14, line[8]);
  i++;
  sprintf(line[9], "  %s   %d    %s\n", status(i), i, step[i]);
  V_line(15, line[9]);
  i++;
  sprintf(line[10], "  %s  %d    %s\n", status(i), i, step[i]);
  V_line(16, line[10]);
  i++;
  sprintf(line[11], "  %s  %d    %s\n", status(i), i, step[i]);
  V_line(17, line[11]);
  i++;
  sprintf(line[12], "         %d    Miscellaneous Command Menu\n", i);
  V_line(18, line[12]);
  i++;
  V_line(20," Option:  ");
  V_ques(&option,'i',20,9,3);
  V_call();

  /* if step one hasn't been completed, we won't allow the other
     steps to be selected  */

        if ((complete[1] == 0) && (option >= 2) && (option <= 12))
            option = 1;

  /* if running option 1 - 10, this makes the answers input null and
     void, so we set that step to 0 */

       if ((complete[11] == 1) && (option >= 1) && (option < 11))
           complete[11] = 2;

  /*  case switch for menu options */

        switch (option)
        {
        case 0:   
                   /*-----------------------
                   ending_details();
                   ------------------------*/
                   save_project();
                   exit(0);
                   break;

        case 1:    step_1();    
                   break;

        case 2:    get_soils();
                   break;

        case 3:    get_cover();
                   break;

        case 4:    get_elevation();
                   break;

        case 5:    get_rain();
                   break;

        case 6:    get_outlet();
                   break;

        case 7:    get_tile();
                   break;

        case 8:    get_chnl();
                   break;

        case 9:    get_chnl_slp();
                   break;

        case 10:   bmp_menu();
                   break;
                   
        case 11:   answers_input();
                   break;

        case 12:   misc();
                   break;

        default:   fprintf(stderr,"\nEnter number between 0 and 12\n");
                   sleep(2);
                   break;
        }
/* after each step is executed, record status of project
   (this way, if a subsequent step being run dies and causes an
   abort, the user doesn't loose everything from that session) */

    save_project();

    }


}
char *
status(number)
    int number;
{
    char buf[80];
    
    if (complete[number] == 0)
        return("     ");
    else if (complete[number] == 1)
        return("done ");
    else if (complete[number] == 2)
        return("rerun");
    else
        {
        sprintf(buf, 
        "could not determine the status of step %d",
        number);
        croak(1, buf);
        }
}
