
#include <grass/config.h>

#ifdef USE_G_SOCKS
#ifndef __MINGW32__

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/raster.h>

#include "open.h"

extern int _rfd, _wfd;
extern int _quiet;

extern int sync_driver(char *);

static char *sockpath;

/*!
 * \brief initialize graphics
 *
 * Initializes connection to
 * current graphics driver. Refer to GRASS User's Manual entries on the
 * <i>d.mon</i> command. If connection cannot be made, the application module
 * sends a message to the user stating that a driver has not been selected or
 * could not be opened. Note that only one application module can be connected to
 * a graphics driver at once.
 * After all graphics have been completed, the driver should be closed.
 *
 *  \param void
 *  \return int
 */

int unlock_driver(int wipeout)
{
	return -1;
}


int R_open_driver(void)
{
    int verbose;
    int try;
    char *name;

    verbose = !_quiet;
    _quiet = 0;

    name = getenv("MONITOR_OVERRIDE");
    if (!name)
	name = G__getenv("MONITOR");

    if (!name)
    {
        if (verbose)           /* #31 Aug 87 - want error stuff */
        {
            fprintf(stderr,_("No graphics monitor has been selected for output.\n"));
            fprintf(stderr,_("Please run \"d.mon\" to select a graphics monitor.\n"));
            exit(-1);
        }
        return(NO_MON);
    }

    /* Get the full path to the unix socket */
    if ((sockpath = G_sock_get_fname(name)) == NULL)
    {
        if (verbose)
        {
            fprintf(stderr, _("Failed to get socket name for monitor <%s>.\n"),
                            name);
        }
        return (NO_MON);
    }

    /* See if the socket exists, if it doesn't no point in trying to
     * connect to it.
     */
    if (!G_sock_exists(sockpath))
    {
        if (verbose)
        {
            fprintf(stderr, _("No socket to connect to for monitor <%s>.\n"),
                            name);
        }
        return (NO_MON);
    }

    /** Used to be a bunch of stuff about locking here.  This is not
     * necessary with sockets, since the server will only accept
     * one connection at a time, all other connections will be refused
     * until it closes it's current connection.
     */

    /** We try to make a connection now **/
    for (try = 0; try < 2; try++)
    {
        _wfd = G_sock_connect(sockpath);
        if (_wfd > 0) /* success */
        {
            _rfd = dup(_wfd);
            sync_driver(name);
            return (OK);
        }
        switch (errno)
        {
            case ECONNREFUSED:
            case EADDRINUSE:
                    if (verbose)
                        fprintf(stderr, _("Socket is already in use or not "
				 "accepting connections.\n"
				 "Use d.mon to select a monitor\n"));
                    return (NO_RUN);
                    break;
            case EBADF:
            case ENOTSOCK:
                    if (verbose)
                        fprintf(stderr, _("Trying to connect to something "
				 "not a socket.\nProbably program "
				 "error.\n"));
                    return (NO_RUN);
                    break;
            case ETIMEDOUT:
                    if (verbose)
                        fprintf(stderr, _("Connect attempt timed out. "
				 "Probably an error with the server.\n"));
                    return (NO_RUN);
                    break;
            default:
                    break;
        }
        fprintf(stderr, _("Not connected...\n"));
        if (verbose && try < 1)
        {
            fprintf(stderr, _("Couldn't connect to monitor. "
		     "Will try once more.\n"));
            G_sleep(1);
        }
        else if (verbose && try > 0)
        {
            fprintf(stderr, _("Connection failed.\n"));
        }
    }
            
    /* We couldn't connect... */
    return (NO_RUN);
}

#else /* __MINGW32__ */


#endif /* __MINGW32__ */
#endif /* USE_G_SOCKS */
