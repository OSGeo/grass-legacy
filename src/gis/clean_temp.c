#include "gis.h"
#include <sys/types.h>
#include <sys/stat.h>
/**************************************************************
 * clean_temp
 *
 *   looks for all files in mapset temp directory
 *   of the form pid.n and removes those which have
 *   been abandoned their processes (pid).
 *
 *   also removes any other file found which is "old"
 **************************************************************/

#include <errno.h>
extern int errno;

main (argc, argv)	char *argv[];
{
    char buf[300];
    char *mapset;
    int pid;
    int uid;
    int n;
    int now;
    struct stat info;
    long max_age;

    FILE *ls, *popen();
    long time();

    G_gisinit ("CLEAN_TEMP");

/* chdir to mapset temp directory */
    G__file_name (buf, ".tmp", "", mapset = G_mapset());
    if (chdir (buf) < 0)
	    exit(0);

/* get user id and current time in seconds */

    uid = getuid () ;
    now = time(0) ;

/* set maximum age in seconds (4 days) */
    max_age = 4 * 24 * 60 * 60 ;

/*
* execute the ls command
* files owned by the user and of the form pid.n
* are removed if the process is not running
*
* all "old" files are removed as well
*/

    printf ("removing old temporary files in mapset %s\n", mapset);
    if (ls = popen ("ls","r"))
    {
	while (G_getl (buf, sizeof buf, ls))
	{
	    if (stat(buf, &info) != 0)
		    continue;
	    if (sscanf (buf, "%d.%d", &pid, &n) == 2 && info.st_uid == uid)
	    {
		if (!find_process (pid))
		{
		    unlink (buf);
		    continue;
		}
	    }
	    if ((now - info.st_ctime) > max_age)
		unlink (buf);
	}

	pclose (ls);
    }
}

find_process (pid)
{
    return (kill (pid, 0) == 0 || errno != ESRCH) ;
}
