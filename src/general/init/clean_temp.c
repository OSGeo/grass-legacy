#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "gis.h"
#include "local_proto.h"
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

#define SLEEP 30	/* 30 minutes */

int 
main (int argc, char *argv[])
{
    char buf[300];
    char *mapset;
    char element[300];
    int ppid;
    int pid;
    int uid;
    int n;
    time_t now;
    struct stat info;
    long max_age;

    FILE *ls, *popen();

    G_gisinit(argv[0]) ;
    ppid = 0;
    if (argc > 1)
	sscanf(argv[1],"%d", &ppid);

/* chdir to mapset temp directory */
    G__temp_element(element);
    G__file_name (buf, element, "", mapset = G_mapset());
    if (chdir (buf) < 0)
	    exit(0);

/* get user id and current time in seconds */

    uid = getuid () ;
    now = time(NULL) ;

/* set maximum age in seconds (4 days) */
    max_age = 4 * 24 * 60 * 60 ;

/*
* execute the ls command
* files owned by the user and of the form pid.n
* are removed if the process is not running
*
* all "old" files are removed as well
*/

    while(1)
    {
	if (ppid > 0 && !find_process(ppid))
	    break;
	/*fprintf (stderr,"Removing old temporary files in mapset %s\n", mapset);*/
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
	if (ppid <= 0)
	    break;
	sleep(SLEEP);
    }
    exit(0);
}

int 
find_process (int pid)
{
    return (kill (pid, 0) == 0 || errno != ESRCH) ;
}

