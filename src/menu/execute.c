/* %W% %G% */
/* this is essentially the system() call, except for the signal
   handling
*/

#include <signal.h>
#include <stdio.h>

execute (s)
char *s;
{
	int status, pid, w;
	int (*sigint)(), (*sigquit)() ;

	sigint  = signal (SIGINT,  SIG_IGN);
	sigquit = signal (SIGQUIT, SIG_IGN);

	fflush (stdout);
	fflush (stderr);

	if ( (pid = fork()) == 0)
	{
		signal (SIGINT,  SIG_DFL);
		signal (SIGQUIT, SIG_DFL);

		execl ("/bin/sh", "sh", "-c", s, 0);
		_exit(127);
	}

	while ( (w = wait (&status)) != pid && w != -1)
		;

	if (w == -1)
		status = -1;

	signal (SIGINT,  sigint);
	signal (SIGQUIT, sigquit);

	return (status);
}
