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

   function:  get_outlet
   called by: menu

   */
#include "answers.h"

get_outlet()
{

    char buf[80];
    char blank[5];
    int not_done =1;

    if (complete[6] > 0)
    {
        printf("\n           ANSWERS on GRASS  Watershed Outlet\n\n");
        printf("This step was previously completed. The current row and\n");
        printf("column number for the watershed outlet is:\n\n");
        printf("             row: %d\n", out_row);
        printf("          column: %d\n\n", out_col);
        if(G_yes("Do you wish to change this?",0))
            complete[6] = 0;
        else
        {
            complete[6] = 1;
            return(0);
        }
    }
 
    strcpy(blank, "    ");
    V_clear();
    V_line(2,"           ANSWERS on GRASS  Watershed Outlet");
    V_line(4,"ANSWERS needs to know the row and column number of the cell at");
    V_line(5,"the watershed outlet. To facilitate your finding this information,");
    sprintf(buf, "The raster map %s.ELEMENT has been created.", proj_name);
    V_line(6, buf);
    V_line(7,"The category values of this map are the sequentially numbered cells");
    V_line(8,"of the watershed. The category descriptions are the cell's row and");
    V_line(9,"column number. Using a tool such as \"d.what.rast\", the row and");
    V_line(10,"column number of the outlet cell can be queried.  ");
    
    V_line(14,"       row: ");
    V_line(15,"    column: ");

    if(complete[6] == 0)
        out_row = out_col = 0;

    while(not_done)
    {
        not_done = 0;
        V_ques(&out_row, 'i', 14, 13, 4);
        V_ques(&out_col, 'i', 15, 13, 4);
        V_intrpt_ok();
        V_intrpt_msg("return to Main Menu");
        if (!V_call())
        {
            if(complete[6] > 0)
                complete[6] = 2;
            return(0);
        }
        
        if((out_row <= 0) || (out_col <= 0))
        {
            V_line(17, "Row and column values must be greater than zero.");
            not_done = 1;
        }
        else
        {
            V_line(17, " ");
        }
        if((out_row > window.rows) || (out_col > window.cols))
        {
            V_line(18, "Value too large for size of project region.");
            V_line(19, "Value limits are      rows and      columns.");
            V_const(&window.rows, 'i', 19, 17, 4);
            V_const(&window.cols, 'i', 19, 32, 4);
            not_done = 1;
        }
        else
        {
            V_line(18, " ");
            V_line(19, " ");
            V_const(blank, 's', 19, 17, 4);
            V_const(blank, 's', 19, 32, 4);
        }
        if(not_done == 1)
            out_row = out_col = 0;
    }
        complete[6] = 1;
        return(0);
        
}
