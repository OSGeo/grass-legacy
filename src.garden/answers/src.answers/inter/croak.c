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

   function: croak
   called by: various
    
    some sort of function to deal with errors.
    code 0: print warning and ask if user wishes to continue
            (in such a case, the calling function will have some
            means of dealing with the problem if the user goes on)
    code 1: print error message, and return to main menu
    code 2: print error message, close things down, and die
  

   */

#include "gis.h"

croak(code,message)
    int code;
    char *message;
{

    fprintf(stderr,"\7");
    if(code == 0)
        {
        fprintf(stderr, "\n\7WARNING: %s\n", message);   
        fprintf(stderr, "\n\n");
        if(G_yes("Shall we try to continue?", 1))
            return(0);
        else
            {
            menu();
            }
        }
    else if(code == 1)
        {
        fprintf(stderr, "\n\7WARNING: %s\n", message);   
        fprintf(stderr, "\n\n");
        hit_return();
        menu();
        }
    else if(code == 2)
        {
        fprintf(stderr, "\n\7ERROR: %s", message);
        fprintf(stderr, "\nANSWERS on GRASS Project Manager aborting\n\n");
/*
        ending_details();
*/
        sleep(3);
        exit(-1);
        }
    return(0);
}
