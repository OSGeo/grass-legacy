/* Program: show.display		Paul W. Carlson		12/88
 */

main(argc, argv)
int argc;
char *argv[];
{
    char *display_path;
    char *display_dir;
    char *get_dir();
    char *get_path();
    static char *prompt = "Enter name of file to be displayed: ";
    
    R_open_driver();

    /* get the directory name containing the saved displays */
    display_dir = get_dir();

    if (argc == 1)
    {
    	/* display the files in the directory */
    	show_dir(display_dir);

    	/* get the file name from the user and create the complete path */
    	display_path = get_path(prompt, display_dir);
    }

    else
    {
    	/* add name to path */
    	strcpy(display_path, display_dir);
    	strcat(display_path, "/");
    	strcat(display_path, argv[1]);
    }

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
