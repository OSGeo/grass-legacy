/* Function: get_path		Paul W. Carlson		12/88
 */

char *get_path(prompt, display_dir)
char *prompt, *display_dir;
{
    char display_path[128];
    char name[40];

    /* prompt user for file name */
    printf("%s", prompt);

    /* get name from user */
    gets(name);

    /* add name to path */
    strcpy(display_path, display_dir);
    strcat(display_path, "/");
    strcat(display_path, name);

    /* return complete path */
    return (display_path);
}
