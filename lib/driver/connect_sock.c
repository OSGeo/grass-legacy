
#include <grass/config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>

#include <grass/gis.h>
#include "driverlib.h"

#ifdef USE_G_SOCKS

int
get_connection_sock(int listenfd, int *rfd, int *wfd, int other_fd)
{
	int fd;

	if (other_fd >= 0)
	{
		fd_set waitset;

		FD_ZERO(&waitset);
		FD_SET(listenfd, &waitset);
		FD_SET(other_fd, &waitset);
		if (select(FD_SETSIZE, &waitset, NULL, NULL, NULL) < 0)
		{
			perror("get_connection_sock: select");
			exit(EXIT_FAILURE);
		}

		if (!FD_ISSET(listenfd, &waitset))
			return -1;
	}

	/* G_sock_accept will block until a connection is requested */
	fd = G_sock_accept(listenfd);
	if (fd >= 0)
	{
		*rfd = fd;
		*wfd = dup(fd);
		return 0;
	}

	if (errno == EINTR)
		return -1;

	G_warning("G_sock_accept: error \"%s\"",
		strerror(errno));
	COM_Graph_close();
	exit(EXIT_FAILURE);
}

int
prepare_connection_sock(const char *me, const char *sockpath)
{
	int fd;

	if ((fd = G_sock_bind(sockpath)) < 0)
	{
		G_fatal_error("Can't bind to socket: error \"%s\"\n",
			      strerror(errno));
	}

	/* Now set up listen */
	if (G_sock_listen(fd, 1) != 0)
	{
		G_fatal_error("G_sock_listen: error \"%s\"\n", 
			      strerror(errno));
	}

	return fd;
}

int
check_connection(const char *me, const char *link)
{
	int fd;

	if (G_sock_exists(link))
	{
		if ((fd = G_sock_connect(link)) >= 0)
		{
			close(fd);
			G_warning("Graphics driver [%s] is already running", me);
			return(-1);
		}
		if (unlink(link) < 0)
		{
			G_warning("Failed to remove stale socket file:"
				"\t%s\n", link);
			return(-2);
		}
	}

	return(0);
}

#endif /* USE_G_SOCKS */

