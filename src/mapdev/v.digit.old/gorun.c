#include	<signal.h>
#include	<stdio.h>
#include "gis.h"

gorun(cmd, args)
	char *cmd ;
	char *args ;
{
	int status, pid, w;
	register int (*istat)(), (*qstat)(), (*tstat)();
	char full_cmd_name[128] ;
	char command[256] ;

	sprintf(full_cmd_name, "%s/bin/%s", G_gisbase(), cmd) ;
	if (-1 == access(full_cmd_name, 1 /*execute OK*/))
	{
		sprintf(command, "%s/bin/%s not available", G_gisbase(), cmd) ;
		G_warning(command) ;
		return(-1) ;
	}
		
	sprintf(command, "%s/bin/%s %s", G_gisbase(), cmd, args) ;
/*
fprintf(stderr,"Calling: %s\n", command) ;
*/

#ifdef SIGTSTP
	tstat = signal(SIGTSTP, SIG_IGN);
#endif
	if ((pid = fork()) == 0) {
		if (0 > set_uid_to_user ())
		    _exit (127);
		execl("/bin/sh", "sh", "-c", command, 0);
		_exit(127);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);

	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
#ifdef SIGTSTP
	signal(SIGTSTP, tstat);
#endif
	return(status);
}
