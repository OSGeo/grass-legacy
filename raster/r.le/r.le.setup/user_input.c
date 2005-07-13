/*
************************************************************
* MODULE: r.le.setup/user_input.c                          *
*         Version 5.0beta            Oct. 1, 2001          *
*				                           *
* AUTHOR: W.L. Baker, University of Wyoming                *
*         BAKERWL@UWYO.EDU                                 *
*                                                          *
* PURPOSE: To set up sampling areas, which can can then    *
*         be used to obtain data using the r.le.dist,      *
*         r.le.patch, and r.le.pixel programs.  The        *
*         user_input.c code displays an error message if   *
*         the user attempts to run r.le.setup on the       *
*         on the command line                              *
*				                           *
* COPYRIGHT: (C) 2001 by W.L. Baker                        *
*                                                          *
* This program is free software under the GNU General      *
* Public License(>=v2).  Read the file COPYING that comes  *
* with GRASS for details                                   *
*				                           *
************************************************************/

#include "setup.h"

int
user_input(argc,argv)
int argc ;
char **argv ;
{

   if (argc > 1) {
      fprintf(stderr, "\nUsage:\n") ;
      fprintf(stderr, "  r.le.setup\n") ;
      fprintf(stderr, "\n   (This command must be run interactively)\n") ;
      
      exit(1) ;
   }
   return;
}
