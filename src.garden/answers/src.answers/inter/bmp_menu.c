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

   function: bmp_menu
   called by: menu 

   provides a menu for running Step 10 (preparing BMP inputs)

   */
#include "answers.h"

bmp_menu()
{

    int option;
    int i;
    int header;
    int line;

    if (complete[10] == 0)
    {
    header = 0;
        for (i = 0; i < 4; i++)
        {
            bmp_tbl[i].set = 0;
            strcpy(bmp_tbl[i].layer, "none");
        }
    }
    else
        header = 1;

/* we have 2 versions of the menu screens, one with text about bmps
   and one that displays the status of bmp. the header variable flags
   which screen to show */

/* create menu with V_ask lib routines. if this step marked
   complete, show status of the 4 menu options, otherwise
   print intro text. */

    while(1)
    {
        option = 0;
        line = 1;
        V_clear();

        V_line(line,
        "       ANSWERS on GRASS  Best Managment Practice (BMP) Utility Menu");
        line = line + 2;

        if (header == 0)
        {
            V_line(line++,
            "Many BMPs can be described to ANSWERS by changing variables describing");
            V_line(line++,
            "the surface condition of the soil. Practices which are tillage-oriented,");
            V_line(line++,
            "for example, are described in the soils and land use sections. Gully");
            V_line(line++,
            "structures such as a drop spillway may be similated by reducing channel");
            V_line(line++,
            "slope. On the other hand, BMPs which are structural in nature require");
            V_line(line++,
            "a change in land use (row crop to grass for waterways, for example).");
            V_line(line++,
            "ANSWERS recognizes four optional BMP structures. Options 1-4 allow you");
            V_line(line++,
            "to define a given BMP usage in the watershed.");
        }
        else
        {
            line++;
            V_line(line++,
            "        BMP Status                        Raster Layer");
            line++;
            for (i = 0; i < 4; i++)
            {
                V_const(bmp_tbl[i].title, 's', line, 9, 20);
                V_const(bmp_tbl[i].layer, 's', line++, 43, 10);
            }
        }
        V_line(11,"MENU: - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
        V_line(12,"        BMP Utility Programs              Menu Egress");
            V_line(14,
            "      1. Tile Outlet Terrace            5. Use NO BMPs");
        if (header == 0)
        {
            V_line(15,
            "      2. Sedimentation Pond             (mark this step \"set\")");
            V_line(16,
            "      3. Grassed Waterway               6. Show BMP Status");
        }
        else
        {
            V_line(15,
            "      2. Sedimentation Pond             (cancel current BMPs)");
            V_line(16,
            "      3. Grassed Waterway               6. About ANSWERS BMPs");
        }
        V_line(17,
        "      4. Field Border                   0. Return to Main Menu");
        V_line(19 ,
        "                         Option:");
        V_ques(&option, 'i', 19, 33, 2);
        V_line(21,
        "  Use options 1-4 to define BMPs in watershed, or 5 if no BMPs are to be used");
        V_intrpt_msg("Return to Main Menu");
        V_intrpt_ok();

        if (! V_call())
            return(0);

        switch(option)
        {
            case 0:
                return(0);
            case 1:
                get_bmp(option);
                break;
            case 2:
                get_bmp(option);
                break;
            case 3:
                get_bmp(option);
                break;
            case 4:
                get_bmp(option);
                break;
            case 5:
                cancel_bmp();
                printf("\n\n\n\n\n");
                complete[10] = 1;
                printf("No BMPs to be used in watershed\n");
                sleep(3);
                return(0);
            case 6:
                if(header==0) header=1;
                else header=0;
                break;
            default:
                fprintf(stderr, "\n\n\n\nPlease choose option 0-6\n");
                sleep(3);
                break;
            }
        if (option == 0)
            break;
        if (option != 6)
            header = 1;
        }
        
    complete[10] = 1;
    return(0);
}

cancel_bmp()
{

    int i;

    save_project();

    for (i = 0; i < 4; i++)
    {
        bmp_tbl[i].set = 0;
        strcpy(bmp_tbl[i].layer, "none");
    }
    G_remove(data_dir, "in_bmp");
    return(0);

}
