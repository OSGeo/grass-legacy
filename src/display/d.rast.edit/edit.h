#include "gis.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif
GLOBAL char new_name[40], current_name[40], orig_name[40];
GLOBAL char grid_color_name[40];
GLOBAL CELL max_value, min_value;
GLOBAL int cellsize;
GLOBAL char user_mapset[40], current_mapset[40], orig_mapset[40];
GLOBAL struct Cell_head real_window;
GLOBAL char *tempfile;
GLOBAL struct Categories cats;
GLOBAL struct Colors colr;
GLOBAL int grid_color;
GLOBAL int real_nrows, real_ncols;
GLOBAL int colr_ok, cats_ok;
GLOBAL int change_made;


/* keeping names straight:

new_name    - new name user has input for the cell
              layer to be created in their mapset
user_mapset - mapset where "new_name" will be created.
              a.k.a. output of G_mapset command
orig_name   - name of the original file (being edited)
              a.k.a. the layer displayed on the monitor
              when the program starts
orig_mapset - the mapset where the "orig_name" layer
              and it's support files are found.
current_name- when editing is taking place, a map is
              displayed on the screen. The first time
              the user is in "edit mode", current_name
              will be same as orig_name. If edit mode
              is exited and then entered again (while 
              still in Dedit), we will have written the
              changes made from the first edit to 
              "new_name" and since we want what is on
              the screen to match the current state of
              edits, we set current_name to the new_name
              and Dcell new_name.
current_mapset - mapset that jives with where the current_name
                 is hanging out.
*/
