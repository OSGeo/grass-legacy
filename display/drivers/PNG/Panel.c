/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.   The last part of the name can be parsed off and used as
 * a pointer name to the saved image.
 */

#include <stdio.h>

#include "pngdriver.h"

int Panel_save(char *name, int top, int bottom, int left, int right)
{
    return -1;
}

int Panel_restore(char *name)
{
    return -1;
}

int Panel_delete(char *name)
{
    remove(name);
    return 0;
}

