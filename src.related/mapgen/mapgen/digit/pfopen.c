#include <stdio.h>
#include <signal.h>

#define WRITE  1
#define READ   0

static int pid;
	FILE *
pfopen(argv, type) char *argv[]; char *type; {
	int p[2];

	if (pipe(p) < 0)
		return (NULL);

	if ((pid = fork()) == 0) { /* new prog */
		if (*type == 'w') {
			close(0); dup(p[READ]);
		} else {
			close(1); dup(p[WRITE]);
		}
		close(p[READ]); close(p[WRITE]);
		execv(argv[0], &argv[1]);
		fprintf(stderr,"can't execute prog:%s\n",
			argv[0]);
		_exit(1); /* disaster occurred */
	} /* old program */
	if (pid == -1) {
		close(p[WRITE]);
		close(p[READ]);
		return (NULL);
	} else if (*type == 'r') {
		close(p[WRITE]);
		return(fdopen(p[READ], type));
	} else {
		close(p[READ]);
		return(fdopen(p[WRITE], type));
	}
}

pfclos(f) FILE *f; {
	register r, (*istat)(), (*qstat)();
	int status;

	fclose(f);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while ((r = wait(&status)) != pid && r != -1) ;
	if (r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);

	return(status);
}
