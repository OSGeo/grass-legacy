/* Function: check_dir		Paul W. Carlson		12/88
 */

check_dir(display_dir)
char *display_dir;
{
    int old_umask;

    /* if directory does not exist, create it */
    if (access(display_dir, 00)) 
    {	old_umask = umask(0);
	mkdir(display_dir, 0777);
    	umask(old_umask);
    }
}
