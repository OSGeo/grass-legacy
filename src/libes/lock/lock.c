/******************************************************************
* lock_file (file,pid)
*   char *file
*
*   this routine "locks" the file for process pid as follows:
*
*   1. if file exists, the old pid is read out of the file.
*
*      if the old pid and the new pid are the same, then
*      nothing more is done and the request is successful.
*
*      if this the old pid process is still running, the file is
*      considered locked.
*
*   2. if file does not exist, or if file exists but process is not
*      running (ie, lock was not removed), the file is locked for
*      process pid by writing pid into the file.
*
*
* note: since file is created by this routine, it shouldn't be
*       a file that is used for any other purpose.
*
*       also, this lock mechanism is advisory. programs must call this
*       routine and check the return status.
*
* returns:
*       1 ok lock is in place.
*       0 could not lock because another process has the file locked already
*      -1 error. could not create the lock file
*      -2 error. could not read the lock file.
*      -3 error. could not write the lock file.
******************************************************************/
#define OK 1
#define ALREADY_LOCKED 0
#define CANT_CREATE -1
#define CANT_READ -2
#define CANT_WRITE -3

#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "lock.h"

extern int errno;

#ifndef __MINGW32__
static int get_pid(char *,int *);
static int find_process(int);
#endif

int lock_file(char *file, int lock_pid)
{
#ifdef __MINGW32__
    /* Bad move but quick fix for the time being */
    return ( OK );
#else    
    int fd;
    int locked;
    int mask;
    int n;
    int old_pid;


    locked = 0;
    if (access (file, 0) == 0) /* file exists */
    {
	for (n = 0; n < 2; n++)
	{
	    if (get_pid (file, &old_pid))
		break;
	    if (n == 0)
		sleep(2); /* allow time for file creator to write its pid */
	}
	if (n == 2)
	    locked = 0;
	else if (lock_pid == old_pid)
	    return OK;
	else
	    locked = find_process (old_pid);
    }
    if (locked)
	return ALREADY_LOCKED;
    mask = umask (0);
    unlink (file);
    fd = creat (file, 0666) ;
    umask (mask);
    if (fd < 0)
	return CANT_CREATE;
    if (write(fd, &lock_pid, sizeof lock_pid) != sizeof lock_pid)
    {
	close (fd);
	return CANT_WRITE;
    }
    close (fd);
    return OK;
#endif    
}

#ifndef __MINGW32__
static int get_pid(
    char *file,
    int *old_pid)
{
    int fd;
    int n;

    if ((fd = open (file, 0)) < 0)
	return 0;
    n = read (fd, old_pid, sizeof (*old_pid));
    close (fd);
    return n == sizeof (*old_pid);
}

static int find_process (int pid)
{
/* attempt to kill pid with NULL signal. if success, then
   process pid is still running. otherwise, must check if
   kill failed because no such process, or because user is
   not owner of process
*/
    if (pid <= 0)
	return 0;
    if (kill (pid, 0) == 0)
	return 1;
    return errno != ESRCH;
}
#endif /* __MINGW32__ */
