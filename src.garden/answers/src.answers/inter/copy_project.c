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

   function: copy_project
   called by: main 

   copy a project in current mapset. the entire project database
   directory (G_mapset()/answers/data/proj_name) is copied to a
   directory with the new project name using the cp -R command.
   A copy of the project file is made by first reading the original
   project file with the read_project function, then switching the
   global var proj_name to the new name and writing the new project
   file with save_project

   Note: it would be nice to copy from someone else's mapset, but
   that isn't all that feasible since the map names used in theirs
   maybe something like "my.soils in workspace"... if the current
   user's mapset "workspace" didn't have that map, they'd be up the
   creek...

   */
#include "answers.h"

copy_project()
{

    char old_proj_name[41];
    char dirname[200];
    char line[500];
    
/* get name of original project */

    proj_mapset = G_ask_in_mapset("Enter the name of the existing project to be copied:\n",
    proj_name, "answers/project", "ANSWERS on GRASS project");

/* user cancels */

    if (!proj_mapset) 
	return(0);
    
/* build path to database directory */

    sprintf(data_dir, "answers/data/%s", proj_name);
    
/* read oringal project file */

    if (read_project() == 1)
    {
         fprintf(stderr,"\n\7WARNING: Could not read existing project data\n"); 
         hit_return();
         return(0);
    }
    
/* copy name */

    strcpy(old_proj_name, proj_name);

/* get name for new project */

    proj_mapset = G_ask_new("Enter new project name:\n", proj_name,
    "answers/project", "ANSWERS on GRASS project");
    
/* user cancels */

    if (!proj_mapset) 
        return(0);

/* build path to new database directory */

    sprintf(data_dir, "answers/data/%s", proj_name);

/* courtesy confirm */

    printf("\n\nCopying project <%s> to <%s>\n", old_proj_name, proj_name);
    if (!G_yes("\nIs this ok? ", 1))
        return(0);

    printf("\nworking...\n");
        
/* write new project file */

    save_project();
    
/* build complete path to project database */

    strcpy(dirname, G_location_path());
    strcat(dirname, "/");
    strcat(dirname, proj_mapset);
    strcat(dirname, "/answers/data");

/* create command to do a recursive copy */

    sprintf(line, "cd %s;cp -R %s %s", dirname, old_proj_name, proj_name);
    
/* let it rip */

    if (G_system(line) != 0)
    {
         fprintf(stderr,"\n\7WARNING: Copy failed.\n"); 
         hit_return();
         return(0);
    }
    
/* make sure we have the right region set */

    set_region();
    
/* trick the set_mask program to think this is the first time
    thru */
    
    complete[1] = 2;
    
/* create element map and reclass rules */

    if (set_mask() == 1)
    {
         fprintf(stderr,"\n\7WARNING: could not set mask or create element map\n"); 
         hit_return();
         return(0);

    }
    
    return(0);

}
