#ifndef lint
static char *SCCSID = "@(#)wpopen.c	USGS v.4.2";
#endif
#include <stdio.h>
#include <signal.h>

#define WRITE  1
#define READ   0

static int pltr_pid;

	FILE *
wpopen(argv) char **argv; {
	int po[2];

	if (pipe(po) == -1)
		return ((FILE *)0);
	if ((pltr_pid = fork()) == 0) { /* new prog */
		fflush(stdin);
		close(0); /* set stdin from father */
		dup(po[READ]);
		close(po[WRITE]);
		execv(argv[0], argv);
		exit(1); /* disaster occurred */
	} /* old program */
	if (pltr_pid == -1)
		return((FILE *)0);
	close(po[READ]);
	return(fdopen(po[WRITE], "w"));
}
wpclose(fi) FILE *fi; {
	int r, status;
	void (*hstat)(), (*istat)(), (*qstat)();

	if ( ! fclose(fi) ) {
		istat = signal(SIGINT, SIG_IGN);
		qstat = signal(SIGQUIT, SIG_IGN);
		hstat = signal(SIGHUP, SIG_IGN);
		while ((r = wait(&status)) != pltr_pid && r != -1) ;
		if (r == -1)
			status = -1;
		signal(SIGINT, istat);
		signal(SIGQUIT, qstat);
		signal(SIGHUP, hstat);
	}
	return(status);
}
