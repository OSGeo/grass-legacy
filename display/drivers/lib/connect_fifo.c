
#include "config.h"

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "driverlib.h"

#ifdef __CYGWIN__
#define MODE 0644
#else
#define MODE 0666
#endif

#ifndef USE_G_SOCKS

static jmp_buf save;

static RETSIGTYPE
timeout(int sig)
{
	longjmp(save, 1);
}

int
get_connection_fifo(char *files, int *rfd, int *wfd, int nonblock)
{
	char input[1024], output[1024];
	RETSIGTYPE (*def)(int);

	if (sscanf(files,"%s %s", input, output) != 2)
	{
		fprintf(stderr, "Invalid connection specification <%s>", files);
		exit(-1);
	}

	if (setjmp(save))
	{
		signal(SIGALRM, def);
		return -1;
	}

	if (nonblock)
	{
		def = signal(SIGALRM, timeout);
		alarm(1);
	}

	*rfd = open(input, O_RDONLY);
	if (*rfd < 0)
	{
		perror(input);
		exit(-1);
	}

	*wfd = open(output, O_WRONLY);
	if (*wfd < 0)
	{
		perror(output);
		exit(-1);
	}

	if (timeout > 0)
	{
                alarm(0);
		signal(SIGALRM, def);
	}

	return 0;
}

/* check_connection - see if we are already running */
/* this goes in the same place as prepare_connection and get_connection. */
/* it is called by SWITCHER first to check whether or not the driver */
/* is already running.  link contains the names of the input and output */
/* fifos (for AT&T, socket and mode for Berkley) separated by a space. */
/* return is -1 if something is using the driver's communication */
/* channel, 0 otherwise. */
/* this is the AT&T version. */

int
check_connection(char *me, char *link)
{
	static int time = 2;	/* time to wait for opens below */
	struct stat buf;
	char in_fifo[1024], out_fifo[1024];
	int in_file = -1, out_file = -1;
	RETSIGTYPE (*def)(int);

	sscanf(link, "%s %s", in_fifo, out_fifo);

	/* Check existence and access of in_fifo */
	if (stat(in_fifo, &buf) < 0)
	{
		fprintf(stderr,"Sorry, <%s> not available\n", in_fifo);
		goto error;
	}
	if (!S_ISFIFO(buf.st_mode))
	{
		fprintf(stderr,"Sorry, <%s> is not a FIFO\n", in_fifo);
		goto error;
	}
	if ((buf.st_mode & MODE) != MODE)
	{
		fprintf(stderr,"Sorry, insufficent permissions on <%s>\n",
			in_fifo);
		goto error;
	}

	/* Check existence and access of out_fifo */
	if (stat(out_fifo, &buf) < 0)
	{
		fprintf(stderr,"Sorry, <%s> not available\n", out_fifo);
		goto error;
	}
	if (!S_ISFIFO(buf.st_mode))
	{
		fprintf(stderr,"Sorry, <%s> is not a FIFO\n", out_fifo);
		goto error;
	}
	if ((buf.st_mode & MODE) != MODE)
	{
		fprintf(stderr,"Sorry, insufficient permissions on <%s>\n",
			out_fifo);
		goto error;
	}

	if (setjmp(save))			/* if timed out waiting below */
	{
		signal(SIGALRM, def);		/* back to normal for timer */
		return(0);			/* tell caller nobody's listening */
	}

	/* else	first time through */
	def = signal(SIGALRM, timeout);		/* where to go when timer goes off */
	alarm(time);				/* set timer */

	in_file = open(in_fifo, O_WRONLY);	/* reading here? */
	out_file = open(out_fifo, O_RDONLY);	/* writing here? */

	alarm(0);				/* turn off alarm */
	signal(SIGALRM, def);			/* and restore normal operation */

	if (in_file >= 0)
		close(in_file);
	if (out_file >= 0)
		close(out_file);

	if (in_file < 0)
	{
		fprintf(stderr,"Error opening <%s>\n", in_fifo);
		goto error;
	}

	if (out_file < 0)
	{
		fprintf(stderr,"Error opening <%s>\n", out_fifo);
		goto error;
	}

	fprintf(stderr,"Graphics driver [%s] is already running\n", me);
	return(-1);				/* tell caller someone's listening */

error:
	fprintf(stderr, "Have GRASS adminstrator check etc/monitorcap file\n");
	return(-2);
}

#endif /* USE_G_SOCKS */

