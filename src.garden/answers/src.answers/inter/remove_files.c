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

   function:  remove_files 
   called by: main
 
   */

#include "answers.h"

remove_files()
{
    char buf[100];
    int i = 0;
    
        printf("\nANSWERS on GRASS Project Removal Utility\n");
        printf("\nThis will remove all files and directories in your GRASS\n");
        printf("database associated with a given ANSWERS project\n");
        
    while(i == 0){
        proj_mapset = G_ask_in_mapset("Enter the name of the project to be removed",
        proj_name, "answers/project","ANSWERS on GRASS project");
        if (!strcmp(proj_name, "")) {
            return(0);
        }

        if (!proj_mapset) {
            fprintf(stderr, "\n\7Could not find project.\n");
            hit_return();
            return(0);
        }
        
        printf("\nworking...\n");

        G_remove("answers/project",proj_name);
        G_remove("answers/data",proj_name);
        printf("\nProject <%s> removed.\n\n\n", proj_name);
        
        sprintf(buf, "Remove the raster layer %s.ELEMENT?", proj_name);
        if(G_yes(buf, 1))
        {
            printf("\n");
            sprintf(buf,  "g.remove rast=%s.ELEMENT", proj_name);
            G_system(buf);
        }
        
        if(!G_yes("\n\nRemove another project?", 0))
            i = 1;
        printf("\n\n");
        }
    return(0);
}
