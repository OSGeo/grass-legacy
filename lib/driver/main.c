#include <grass/config.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <errno.h>

#include <grass/gis.h>
#include <grass/glocale.h>
#include "driverlib.h"
#include "driver.h"
#include "pad.h"

static jmp_buf save;

static RETSIGTYPE handle_sigpipe(int sig)
{
	longjmp(save, 1);
}

static RETSIGTYPE handle_sigterm(int sig)
{
	COM_Graph_close();
}

int LIB_main(int argc, char **argv)
{
	char *me;
	char *connpath;
	int _wfd;
	int _rfd;
	int eof;

	char c;
	pid_t pid;
	int foreground;
#ifdef USE_G_SOCKS
	int listenfd;
#endif
	struct sigaction sigact;

	/* The calling syntax is as follows:
	   monitor_name [-] "input_fifo output_fifo"

	   The "-", if present, causes the monitor to run in foreground.
	   Otherwise, once it has been determined that the monitor is not
	   already running, we will fork and the parent will exit, so that
	   the monitor is left running in background.  For more information,
	   see the comments in Dstart_mon.c.
	*/

	if (argc != 4)
	{
		G_warning("Usage:  %s <name> [-] \"input_fifo output_fifo\"\n", argv[0]);
		return 1;
	}

	/* whoami */
	me = argv[1];

	foreground = (argv[2][0] == '-');

#ifdef USE_G_SOCKS
	connpath = G_sock_get_fname(me);
	if (connpath == NULL)
		G_fatal_error("In %s: Couldn't get socket path.\n", __FILE__);
#else
	connpath = argv[3];
#endif

	/* Syntax is all right.  Now we must check and see whether or not someone */
	/* (possibly another invocation of ourself) is using our socket/FIFOs.    */

	if (check_connection(me, connpath) != 0)
	{
		/* Our socket/FIFOs are in use (or other error) */
		G_fatal_error("Unable to start monitor <%s>", me);
	}

	/* We are free to run now.  No one is using our socket/FIFOs.             */
	/* If we are to run in background, we will have foreground == 0 from      */
	/* the syntax check above. */

#ifdef SIGPIPE
	sigact.sa_handler = handle_sigpipe;
	sigemptyset(&sigact.sa_mask);
	sigact.sa_flags = 0;
	sigaction(SIGPIPE, &sigact, NULL);
#endif
	sigact.sa_handler = handle_sigterm;
	sigemptyset(&sigact.sa_mask);
	sigact.sa_flags = 0;
	sigaction(SIGTERM, &sigact, NULL);

#ifdef USE_G_SOCKS
	listenfd = prepare_connection_sock(me, connpath);
#endif

	G_message(_("Graphics driver [%s] started"), me);

	if (!foreground)
	{
		pid = fork();
		if (pid)
		{
			if (pid > 0)        /* parent exits */
			{
				exit(0);
			}
			else    /* weren't able to fork */
			{
				G_fatal_error("Error - Could not fork to start [%s]",me);
				exit(EXIT_FAILURE);
			}
		}
		else
		{
			/* change process groups to be shielded from keyboard signals */
			/* note: use BSD form of call, which will also work for ATT   */
#ifdef SETPGRP_VOID
			setpgrp();
#else
			setpgrp(0, getpid());
#endif
		}
	}               /* monitor runs */

	while (1)   /* re-open upon EOF */
	{
		for (;;)
		{
#ifdef USE_G_SOCKS
			if (get_connection_sock(listenfd, &_rfd, &_wfd, COM_Work_stream()) >= 0)
				break;
#else
			if (get_connection_fifo(connpath, &_rfd, &_wfd, COM_Has_work()) >= 0)
				break;
#endif

			COM_Do_work(0);
		}

		command_init(_rfd, _wfd);

		COM_Client_Open();

		eof = 0;

		while (eof <= 0)
		{
			COM_Do_work(1);

			if (setjmp(save))
			{
				G_warning("Monitor <%s>: Caught SIGPIPE", me);
				break;
			}

			if (get_command(&c) != 0)
				break;

			if (process_command(c))
			{
				G_warning( "Monitor <%s>: Premature EOF", me);
				break;
			}
		}

		/* read encountered EOF. close socket/FIFOs now */
		close(_wfd);
		close(_rfd);
		_rfd = _wfd = -1;  /* Set to invalid file descriptor number */

		COM_Client_Close();
	}

	return 0;
}

