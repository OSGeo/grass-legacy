#ifndef lint
static char *SCCSID = "@(#)mcall.c	AMG v.3.1";
#endif
# include <signal.h>
# include <stdio.h>
# include <fcntl.h>
# include "mapgen.h"
# include "mapdef.h"

mcall(path, argv, sysi, syso) char *path, *argv[], *sysi, **syso; {
	void (*istat)(), (*qstat)();
	int status, pid, w, tty, si, so;

	fflush(stdout);
	if ((*syso = tempnam(TEMP,"m")) == NULL) {
		perror(*syso);
		quit("mcall - sysout name",0);
		return -1;
	}
	if (
		((tty = open("/dev/tty", O_RDWR)) == -1) ||
		((si = open(sysi, O_RDONLY)) == -1) ||
		((so = creat(*syso, 0666)) == -1)  ) {
			perror("open/creat failure");
			quit("mcall - opennings",0);
			return -1;
	}
	if ((pid = fork()) == 0) { /* son */
		close(0);	dup(si);	close(si);
		close(1);	dup(so);	close(so);
		close(2);	dup(tty);	close(tty);
		execv(path, argv);
		exit(127);
	} /* end son */
	close(tty);	close(si);	close(so);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	/* wait */
	while ((w = wait(&status)) != pid && w != -1) ;
	if (w == -1)
		status = -1;
	else if (!(status & 0xff))
		status = (status >> 8) & 0xff;
	else
		status = 0x100 + (status & 0xff);
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);

	return (status);
}
