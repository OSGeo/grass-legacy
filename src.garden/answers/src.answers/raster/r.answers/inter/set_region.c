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

   function: set_region
   called by: main and copy_project

   read the project region and resolution file, set and put
   what it finds as current window, uh, I mean, region.

   */
#include "answers.h"

set_region()
{

    FILE *fd;
    char *G__read_Cell_head();
/*
    printf("\n\nReading region and grid resolution for project\n\n");
*/
    if(!(fd = G_fopen_old(data_dir, "region", proj_mapset)))
    {
        fprintf(stderr, "\n\7could not open <region> file in project database");
        return(1); 
    }
    G__read_Cell_head (fd, &window, 0);
    fclose(fd);

    G_set_window(&window);
    G_put_window(&window);

    return(0);
}
