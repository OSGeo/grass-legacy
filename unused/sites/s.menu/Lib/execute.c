/* this is essentially the system() call, except for the signal
   handling
*/

#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int execute (char *s)
{
	int status, pid, w;
	void (*sigint)(), (*sigquit)(), (*sigtstp)() ;

	sigint  = signal (SIGINT,  SIG_IGN);
	sigquit = signal (SIGQUIT, SIG_IGN);
	/*
	sigtstp = signal (SIGTSTP, SIG_IGN);
	*/

	fflush (stdout);
	fflush (stderr);

	if ( (pid = fork()) == 0)
	{
		signal (SIGINT,  SIG_DFL);
		signal (SIGQUIT, SIG_DFL);
		/*
		signal (SIGTSTP, SIG_DFL);
		*/

		execl ("/bin/sh", "sh", "-c", s, 0);
		_exit(127);
	}

	while ( (w = wait (&status)) != pid && w != -1)
		;
	
	if (w == -1)
		status = -1;

	signal (SIGINT,  sigint);
	signal (SIGQUIT, sigquit);
	/*
	signal (SIGTSTP, sigtstp);
	*/

	return (status);
}
