/* Function: show_dir		Paul W. Carlson		12/88
 */
#include <stdio.h>

show_dir(display_dir)
char *display_dir;
{
    static char sys_cmd[128] = { 'l', 's', ' ', '-', 'C', ' ' };
    static char *dashes = "---------------------------------------";

    printf("\n\nSaved 6386 GRASS screens:\n%s%s\n", dashes, dashes);
    fflush(stdout);
    strcat(sys_cmd, display_dir);
    system(sys_cmd);
    printf("%s%s\n\n", dashes, dashes);
}
