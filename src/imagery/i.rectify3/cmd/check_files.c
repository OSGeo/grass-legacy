/*======================================================================
Filename: check_files.c
Moudle:   i.rectify3  (cmd)
Author:   Mike Baba



   check_files(struct Ref ref,  tRect_Data *rect_data)
     Check that source imagery files from the parser is valid.
     Check if target files can be overwirtten if they exist.
     Put the stuff from rect_data into ref

   Return Value:
      1    OK.
      Problems print message and call G_fatal_error()

Modifications:
30 Oct 93    - mbaba       - original 
TODO Finnish this 

======================================================================*/

#include "global.h"
#include "parse.h"

/** TODO - parser only works with one rast=from,to line **/
#define  NFILES 15


int 
check_files (
    struct Ref ref,             /* group reference structure */
    tRect_Data *rect_data       /* rectify data from parser */
)
{
    char *name=NULL, *mapset;
    char msg[80];
    int  i, source_ok = 0;


    /* check that the source file is in the group */
    for (i=0; i < ref.nfiles; i++)  {

      /* is this the one selected */
      if ( (strcmp (ref.file[i].name, rect_data->source_name)) != 0)
	continue;

      name   = ref.file[i].name;
      mapset = ref.file[i].mapset;

      /* check that the raster still exists */
      if (! G_find_cell (name, mapset)) {
	sprintf (msg, "Source image rast file %s does not exist \n", name);
	G_fatal_error (msg);
      }

      source_ok = 1;

      /* add this to the ref_list, and save the target name */
      /* ref_list and new_name are global.  TODO change it */
      ref_list[i] = i;
      new_name[i] = rect_data->target_name;
    }
    

    /* did we find it */
    if (source_ok == 0) {
	sprintf (msg, "Source image %s not found in group %s\n", 
		 name, group.name);
	G_fatal_error (msg);
    }


    /* check target file for illegal name */
    if (G_legal_filename (rect_data->target_name) < 0) {
      sprintf (msg, "Target file %s is an illegal name\n", 
	       rect_data->target_name);
      G_fatal_error (msg);
    }

    /* check for existing cell files
     * this check must occur in the target location, so we switch
     * environments to be in the target location
     */
    select_target_env();

    if ((G_find_cell (rect_data->target_name, G_mapset() )) &&
	(rect_data->overwrite == 0)) {
     
        sprintf (msg, "Target file %s exists and overwrite flag was not set\n", 
		 rect_data->target_name);
	G_fatal_error (msg);
    }

    select_current_env();
    return (1);
}


