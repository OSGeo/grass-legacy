/* Program: delete	Paul W. Carlson		12/88
 */

main(argc, argv)
int argc;
char *argv[];
{
    char *display_path;
    char *display_dir;
    char *get_dir();
    
    R_open_driver();

    /* get the directory name to contain the saved display */
    display_dir = get_dir();

    /* get the file name and create the complete path */
    strcpy(display_path, display_dir);
    strcat(display_path, "/");
    strcat(display_path, argv[1]);

    /* if the file does not exist, print message and exit */
    if (access(display_path, 00)) 
    	printf("\n\nFile \"%s\" does not exist\n", display_path);

    /* if the file exists and belongs to user, delete it */
    else if (access(display_path, 02) == 0) unlink(display_path);

    /* if file does not belong to user, do not delete it */
    else printf("\nYou do not have permission to delete this file.\n");
    R_close_driver();
}
