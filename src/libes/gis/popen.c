#include <unistd.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>

#ifndef __MINGW32__
#include <sys/wait.h>
#endif
#include "gis.h"

#define tst(a,b)        (*mode == 'r'? (b) : (a))

#define READ      0
#define WRITE     1

static  int     popen_pid[50];

FILE *G_popen(
    char    *cmd,
    char    *mode)
{
    int p[2];
    int me, you, pid;

    fflush (stdout);
    fflush (stderr);

    if(pipe(p) < 0)
	return NULL;
    me = tst(p[WRITE], p[READ]);
    you = tst(p[READ], p[WRITE]);
    if((pid = fork()) == 0)
    {
/* me and you reverse roles in child */
	close(me);
	close(tst(0, 1));
	dup(you);
	close(you);
	execl("/bin/sh", "sh", "-c", cmd, 0);
	_exit(1);
    }

    if(pid == -1)
	return NULL;
    popen_pid[me] = pid;
    close(you);

    return(fdopen(me, mode));
}

int G_pclose( FILE *ptr)
{
#ifdef __MINGW32__
    void (*sigint)();
#else    
    void (*sighup)(), (*sigint)(), (*sigquit)();
#endif
    int f, r;
    int status;

    f = fileno(ptr);
    fclose(ptr);

    sigint  = signal(SIGINT, SIG_IGN);
#ifndef __MINGW32__
    sigquit = signal(SIGQUIT, SIG_IGN);
    sighup  = signal(SIGHUP, SIG_IGN);
#endif

    while((r = wait(&status)) != popen_pid[f] && r != -1)
	    ;

    if(r == -1)
	status = -1;

    signal(SIGINT, sigint);
#ifndef __MINGW32__
    signal(SIGQUIT, sigquit);
    signal(SIGHUP, sighup);
#endif

    return(status);
}
