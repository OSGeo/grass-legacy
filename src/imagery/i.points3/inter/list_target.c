/*=======================================================================
				i.points
  find.c --

     int find_taget_files (void)
          Run etc/i.find command find all cell and vect files
          in the target location.  Write the files found to 
	  cell_list and vect_list respectively.
 
=======================================================================*/

#include <stdlib.h>
#include <unistd.h>
#include "globals.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
#else
#endif

/*---------------------------------------------------------------------*/
int find_target_files(void)
{
    char command[1024];

    select_target_env();
    sprintf (command, "%s/etc/i.find %s %s cell %s dig %s",
	G_gisbase(), G_location(), G_mapset(), cell_list, vect_list);
    select_current_env();

    system(command);

    return 0;
}
