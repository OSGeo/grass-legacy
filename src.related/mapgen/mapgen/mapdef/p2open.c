#ifndef lint
static char *SCCSID = "@(#)p2open.c	AMG v.3.1";
#endif
#include <stdio.h>
#include <signal.h>

#define WRITE  1
#define READ   0

static int pid;

p2open(argv, fi, fo) char *argv[]; int *fi, *fo; {
	int po[2], pi[2];

	if (pipe(po) < 0)
		return (-1);
	if (pipe(pi) < 0) {
		close(po[READ]);
		close(po[WRITE]);
		return (-1);
	}
	if ((pid = fork()) == 0) { /* new prog */
		close(0); dup(po[READ]);
		close(1); dup(pi[WRITE]);
		execv(argv[0], &argv[1]);
		fprintf(stderr,"can't execute prog:%s\n",
			argv[0]);
		_exit(1); /* disaster occurred */
	} /* old program */
	close(po[READ]);
	close(pi[WRITE]);
	if (pid == -1) {
		close(po[WRITE]);
		close(pi[READ]);
	} else {
		*fi = pi[READ];
		*fo = po[WRITE];
	}
	return(pid);
}

p2clos(fi, fo) {
	int r;
	void (*istat)(), (*qstat)();
	int status;

	write(fo, &status, 1);
	close(fo);
	close(fi);

	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while ((r = wait(&status)) != pid && r != -1) ;
	if (r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);

	return(status);
}
