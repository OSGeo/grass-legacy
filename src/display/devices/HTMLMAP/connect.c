#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "driverlib.h"

static jmp_buf save;
static void timeout (void)
{
	longjmp(save,-1);
}

int get_connection (char *files, int *rfd, int *wfd)
{
	char input[1024], output[1024];
	sscanf(files,"%s %s",input,output);
	*rfd = open(input, 0) ;
	if (*rfd == -1)
	{
		perror (input);
		exit(-1) ;
	}

	*wfd = open(output, 1) ;
	if (*wfd == -1)
	{
		perror (output);
		exit(-1) ;
	}

	return 0;
}

int prepare_connection (void)
{

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


int check_connection (char *me, char *link)
{
	struct stat buf ;
	char in_fifo[1024], out_fifo[1024];
	int time, in_file, out_file;
	void (*def)();
	int err ;

	time = 2;			/* time to wait for opens below */
	sscanf(link,"%s %s",in_fifo,out_fifo);
/* Check existence and access of in_fifo */
	err = 0 ;
	if (-1 == stat(in_fifo, &buf))
	{
		fprintf(stderr,"Sorry, <%s> not available\n",in_fifo) ;
		goto error ;
	}
	if (!(buf.st_mode & S_IFIFO))
	{
		fprintf(stderr,"Sorry, <%s> is not a fifo file\n",in_fifo) ;
		goto error ;
	}
	if ((buf.st_mode & 0666) != 0666)
	{
		fprintf(stderr,"Sorry, permissions on <%s> (%o) should be 0666\n",
			in_fifo, buf.st_mode & 0666) ;
		goto error ;
	}
	if (-1 == stat(out_fifo, &buf))
	{
		fprintf(stderr,"Sorry, <%s> not available\n",out_fifo) ;
		goto error ;
	}
/* Check existence and access of out_fifo */
	if (!(buf.st_mode & S_IFIFO))
	{
		fprintf(stderr,"Sorry, <%s> is not a fifo file\n",out_fifo) ;
		goto error ;
	}
	if ((buf.st_mode & 0666) != 0666)
	{
		fprintf(stderr,"Sorry, permissions on <%s> (%o) should be 0666\n",
			out_fifo, buf.st_mode & 0666) ;
		goto error ;
	}

	if (setjmp(save))			/* if timed out waiting below */
	{
		signal(SIGALRM,def);		/* back to normal for timer */
		return(0);				/* tell caller nobody's listening */
	}
	/* else	first time through */
	def = signal(SIGALRM,timeout);	/* where to go when timer goes off */
	alarm(time);				/* set timer */
	out_file = open(in_fifo,O_WRONLY);	/* reading here? */
	in_file = open(out_fifo,O_RDONLY);	/* writing here? */
	alarm(0);				/* turn off alarm */
	signal(SIGALRM,def);			/*   and restore normal operation */
	close(out_file);
	close(in_file);
        fprintf(stderr,"Graphics driver [%s] is already running\n", me);
        fflush (stderr);
	return(-1);				/* tell caller someone's listening */
error:
	fprintf(stderr,"Have GRASS adminstrator check etc/monitorcap file\n") ;
        fflush (stderr);
	return(-2) ;

}

