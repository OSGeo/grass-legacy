/* Function: get_dir		Paul W. Carlson		12/88
 */

char *get_dir()
{
    char display_dir[128];
    char *G__getenv();

    strcpy(display_dir, G__getenv("GISDBASE"));
    strcat(display_dir, "/SCREENS");
    return (display_dir);
}
