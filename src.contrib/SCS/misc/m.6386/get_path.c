/* Function: get_path		Paul W. Carlson		12/88
 */

char *get_path(display_dir)
char *display_dir;
{
    char display_path[128];

    /* add name to path */
    strcpy(display_path, display_dir);
    strcat(display_path, "/");
    strcat(display_path, name);

    /* return complete path */
    return (display_path);
}
