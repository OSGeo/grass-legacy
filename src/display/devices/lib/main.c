#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <errno.h>
#define SWITCHER
#include "gis.h"
#include "graph.h"
#include "driverlib.h"
#include "driver.h"
#include "pad.h"

static jmp_buf save;

static RETSIGTYPE
handle_sigpipe(int sig)
{
    longjmp(save, 1);
}

static RETSIGTYPE
handle_sigterm(int sig)
{
    Graph_Close();
}

#ifdef USE_G_SOCKS
#define EXTRA_PARAMS 0
#else
#define EXTRA_PARAMS 1
#endif

int 
main(int argc, char *argv[])
{
    char *me;
    char *connpath;
    int _wfd;
    int _rfd;
    int nlev;
    int eof;

    char c;
    pid_t pid;
    int foreground, param, syntax;
#ifdef USE_G_SOCKS
    int listenfd;
#endif
    struct sigaction sigact;

    /* whoami */
    me = argv[0];

    /* The calling syntax is as follows:
          monitor_name [-] "input_fifo output_fifo" [nlev] (FIFOs)
          monitor_name [-] [nlev]                          (socket)
       The "-", if present, causes the monitor to run in foreground.
       Otherwise, once it has been determined that the monitor is not
       already running, we will fork and the parent will exit, so that
       the monitor is left running in background.  For more information,
       see the comments in Dstart_mon.c.
    */

    syntax = 0;

    if (argc > 1 && argv[1][0] == '-' && argv[1][1] == '\0')
    {
        foreground = 1;
        param = 2;
    }
    else
    {
        foreground = 0;
        param = 1;
    }

    if (argc == param + EXTRA_PARAMS + 1)
	nlev = atoi(argv[param + EXTRA_PARAMS]);
    else if (argc == param + EXTRA_PARAMS)
	nlev = 32;
    else
	syntax = 1;

    if (syntax)
    {
#ifdef USE_G_SOCKS
        fprintf(stderr,"Usage:  %s [-] [<nlev>]\n",me);
#else
        fprintf(stderr,"Usage:  %s [-] \"input_fifo output_fifo\" [<nlev>]\n",me);
#endif
        exit(-1);
    }

#ifdef USE_G_SOCKS
    connpath = G_sock_get_fname(me);
    if (connpath == NULL)
	    G_fatal_error("In %s: Couldn't get socket path.\n", __FILE__);
#else
    connpath = argv[param];
#endif

    /* Syntax is all right.  Now we must check and see whether or not someone */
    /* (possibly another invocation of ourself) is using our socket/FIFOs.    */

    if (check_connection(me, connpath) != 0)
    {
	/* Our socket/FIFOs are in use (or other error) */
	G_fatal_error("Unable to start monitor <%s>", me);
    }

    /* initialize graphics */
    if ( nlev < -1 ) {
	fprintf(stderr,"Nlev is negative, resetting to 32\n");
	nlev=32;
    }
    if ( nlev > 256 ) {
	fprintf(stderr,"Nlev is too big ( > 256), resetting to 32\n");
	nlev=32;
    }
    if (Graph_Set(argc, argv, nlev) < 0)
        exit(-1);

    /* Initialize color stuff */
    Color_table_fixed();

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

    /* initialize the pads */
    create_pad("");    /* scratch pad */

    if (!foreground)
    {
#ifndef __W98__
        if (pid = fork())
#else
        if (pid = 1)
#endif
        {
            if (pid > 0)        /* parent exits */
            {
                fprintf(stderr,"Graphics driver [%s] started\n", me);
#ifndef __W98__
                exit(0);
#endif
            }
            else    /* weren't able to fork */
            {
                fprintf(stderr,"Error - Could not fork to start [%s]\n",me);
                exit(-1);
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
	    if (get_connection_sock(listenfd, &_rfd, &_wfd, Has_work()) >= 0)
		break;
#else
	    if (get_connection_fifo(connpath, &_rfd, &_wfd, Has_work()) >= 0)
		break;
#endif

	    Do_work(0);
	}

	command_init(_rfd, _wfd);

	Client_Open();

        eof = 0;

        while (eof <= 0)
        {
            Do_work(1);

            if (setjmp(save))
            {
                fprintf(stderr, "Monitor <%s>: Caught SIGPIPE\n", me);
                break;
            }

            if (get_command(&c) != 0)
                break;

            if (process_command(c))
            {
                fprintf(stderr, "Monitor <%s>: Premature EOF\n", me);
                break;
            }
        }

        /* read encountered EOF. close socket/FIFOs now */
        close(_wfd);
        close(_rfd);
        _rfd = _wfd = -1;  /* Set to invalid file descriptor number */

	Client_Close();
    }

    return 0;
}

