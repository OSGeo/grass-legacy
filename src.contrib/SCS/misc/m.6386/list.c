/* 
 * This program lists the saved GRASS displays in the current SCREEN
 * directory that were created with the m.6386.save program.
 */
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>

main()
{
    char *display_dir;
    char *get_dir();

    /* get the directory name to contain the saved display */
    display_dir = get_dir();

    /* display the files in the directory */
    show_dir(display_dir);

}
