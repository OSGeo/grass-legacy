#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/wait.h>
#endif
#include "gis.h"
#include "glocale.h"
/****************************************************************
 * G_system (command)
 *     char *command;
 *
 * This is essentially the UNIX system() call, except for the signal
 * handling. During the call, user generated signals (intr, quit)
 * for the parent are ignored, but allowed for the child. Parent
 * signals are reset upon completion.
 *
 * This routine is useful for menu type programs that need to run
 * external commands and allow these commands to be interrupted by
 * the user without killing the menu itself.
 *
 * Note: if you want the signal settings to be the same for the
 * parent and the command being run, set them yourself and use
 * the UNIX system() call instead.
 ****************************************************************/

#include <signal.h>
#include <stdio.h>


/*!
 * \brief run a shell level command
 *
 * The shell level
 * <b>command</b> is executed. Interrupt signals for the parent module are
 * ignored during the call. Interrupt signals for the <b>command</b> are
 * enabled. The interrupt signals for the parent are restored to their previous
 * settings upon return.
 * G_system(~) returns the same value as system(~), which is essentially the
 * exit status of the <b>command.</b> See UNIX manual system(1) for details.
 *
 *  \param command
 *  \return int
 */

int G_system ( char *command)
{
    int status, pid, w;
    void (*sigint)()
#ifdef SIGQUIT
        , (*sigquit)()
#endif
            ;

    sigint  = signal (SIGINT,  SIG_IGN);
#ifdef SIGQUIT
    sigquit = signal (SIGQUIT, SIG_IGN);
#endif

    fflush (stdout);
    fflush (stderr);

#ifdef __MINGW32__
    signal (SIGINT,  SIG_DFL);
    _spawnl ( P_WAIT,
              "command",
              "command",
              "/c",
              command,
              NULL );
#else    
    if ( (pid = fork()) == 0)
    {
	signal (SIGINT,  SIG_DFL);
	signal (SIGQUIT, SIG_DFL);
    
	execl ("/bin/sh", "sh", "-c", command, NULL);
	_exit(127);
    }

    if (pid < 0)
    {
	G_warning (_("Can not create a new process!"));
	status = -1;
    }
    else
    {
	while ( (w = wait (&status)) != pid && w != -1);

	if (w == -1)
	    status = -1;
    }

#endif

    signal (SIGINT,  sigint);
#ifdef SIGQUIT
    signal (SIGQUIT, sigquit);
#endif

    return (status);
}
