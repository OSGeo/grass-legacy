/* Program: show.display		Paul W. Carlson		12/88
 */

main(argc, argv)
int argc;
char *argv[];
{
    char *display_path;
    char *display_dir;
    char *get_dir();
    
    R_open_driver();

    /* get the directory name containing the saved displays */
    display_dir = get_dir();

    /* get the file name and create the complete path */
    strcpy(display_path, display_dir);
    strcat(display_path, "/");
    strcat(display_path, argv[1]);

    /* if the file exists, display it */
    if (access(display_path, 00) == 0) 
    {	printf("\n\nDisplaying file \"%s\"\n", display_path);
	Dclearscreen();
	R_panel_restore(display_path);
    }

    /* if file does not exit, display message and exit */
    else printf("\nThe file \"%s\" does not exist.\n\n", display_path);
    R_close_driver();
}
