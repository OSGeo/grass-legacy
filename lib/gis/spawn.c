#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <stdarg.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "config.h"
#include "gis.h"
#include "glocale.h"

/****************************************************************
 * G_spawn(char *command, ...)
 *
 * This is a more useful alternative to G_system(), which takes
 * the command's arguments as separate parameters.
 *
 ****************************************************************/

#define MAX_ARGS 32

int G_spawn(char *command, ...)
{
	sigset_t mask, oldmask;
	struct sigaction act, intr, quit;
	int status = -1;
	pid_t pid;
	char *args[MAX_ARGS];
	va_list va;
	int i;

	va_start(va, command);

	args[i] = command;
	for (i = 1; i < MAX_ARGS; i++)
		if (args[i] = va_arg(va, char *), !args[i])
			break;

	va_end(va);

	if (i == MAX_ARGS)
	{
		G_warning(_("too many arguments"));
		return -1;
	}

	act.sa_handler = SIG_IGN;
	act.sa_flags = 0;
	sigemptyset(&act.sa_mask);

	if (sigaction(SIGINT, &act, &intr) < 0)
		goto error_1;
	if (sigaction(SIGQUIT, &act, &quit) < 0)
		goto error_2;

	sigemptyset(&mask);
	sigaddset(&mask, SIGCHLD);
	if (sigprocmask(SIG_BLOCK, &mask, &oldmask) < 0)
		goto error_3;

	pid = fork();

	if (pid < 0)
		G_warning(_("unable to create a new process"));
	else if (pid == 0)
	{
		signal(SIGINT,  SIG_DFL);
		signal(SIGQUIT, SIG_DFL);

		execvp(command, args);
		G_warning(_("unable to execute command"));
		_exit(127);
	}
	else
	{
		pid_t n;

		do n = waitpid(pid, &status, 0);
		while (n == (pid_t) -1 && errno == EINTR);

		if (n != pid)
			status = -1;
	}

	sigprocmask(SIG_SETMASK, &oldmask, NULL);
error_3:
	sigaction(SIGQUIT, &quit, NULL);
error_2:
	sigaction(SIGINT, &intr, NULL);
error_1:
	return status;
}

