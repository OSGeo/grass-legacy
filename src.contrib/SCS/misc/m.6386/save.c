/* Program: save.display		Paul W. Carlson		12/88
 */

main(argc, argv)
int argc;
char *argv[];
{
    int r, b;
    char *display_path;
    char *display_dir;
    char *get_dir();
    char reply[8];
    
    R_open_driver();

    /* get right and bottom of screen */
    r = R_screen_rite();
    b = R_screen_bot();

    /* get the directory name to contain the saved display */
    display_dir = get_dir();

    /* if the directory does not exist, create it */
    check_dir(display_dir);

    /* get the file name and create the complete path */
    strcpy(display_path, display_dir);
    strcat(display_path, "/");
    strcat(display_path, argv[1]);

    /* if the file does not exist, create it */
    if (access(display_path, 00)) 
    {	printf("\n\nSaving display in file \"%s\"\n", display_path);
	R_panel_save(display_path, 0, b, 0, r);
    }

    /* if the file exists and belongs to user, allow overwrite */
    else if (access(display_path, 02) == 0)
    {   reply[0] = '\0';
    	while (reply[0] != 'y' && reply[0] != 'n')
    	{   printf("\nFile exists - overwrite it (y/n)? ");
	    gets(reply);
    	}
    	if (reply[0] = 'y')
    	{   unlink(display_path);
	    R_panel_save(display_path, 0, b, 0, r);
    	}
    }

    /* if file does not belong to user, do not allow overwrite */
    else printf("\nYou do not have permission to overwrite this file.\n");
    R_close_driver();
}
