#ifndef lint
static char *SCCSID = "@(#)p2bopen.c	USGS v.4.1";
#endif
/* double pipe link to 'plotter' */
#include <stdio.h>
#include <signal.h>

#define WRITE  1
#define READ   0

static int pltr_pid;
static int spipe;
static char chans[20];

p2bopen(argv, fi, fo) char *argv[]; FILE **fi, **fo; {
	char *sp;
	int po[2], pi[2];

	for (spipe = 0; (sp = argv[spipe]) &&
		!(*sp == '.' && sp[1] == '\0') ; spipe++) ;
	if (sp) {
		if (pipe(po) < 0) {
			spipe = 0;
			return (-1);
		}
		if (pipe(pi) < 0) {
			close(po[READ]);
			close(po[WRITE]);
			spipe = 0;
			return (-1);
		}
	} else spipe = 0;

	if ((pltr_pid = fork()) == 0) { /* new prog */
		if (spipe) {
			sprintf(chans,"%d.%d",po[READ],pi[WRITE]);
			argv[spipe] = chans;
			close(po[WRITE]);
			close(pi[READ]);
		}
		execv(argv[0], &argv[1]);
		fprintf(stderr,"can't execute prog:%s\n",
			argv[0]);
		_exit(1); /* disaster occurred */
	} /* old program */

	if (pltr_pid == -1)
		return(-1);
	if (spipe) {
		close(po[READ]);
		close(pi[WRITE]);
		/* set to buffered I/O */
		*fi = fdopen(pi[READ], "r");
		*fo = fdopen(po[WRITE], "w");
		argv[spipe] = sp;
		return(0);
	} else return(p2bclos());
}
p2bclos(fi, fo) FILE *fi, *fo; {
	int r;
	void (*hstat)(), (*istat)(), (*qstat)();
	int status;

	if (spipe) {
		spipe = 0;
		fclose(fo);
		fclose(fi);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while ((r = wait(&status)) != pltr_pid && r != -1) ;
	if (r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);

	return(status);
}
