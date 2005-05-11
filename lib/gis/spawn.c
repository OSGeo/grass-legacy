#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>

#ifndef __MINGW32__
#include <sys/wait.h>
#endif
#include "config.h"
#include "gis.h"
#include "glocale.h"
#include "spawn.h"

#define MAX_ARGS 256
#define MAX_BINDINGS 256
#define MAX_SIGNALS 32
#define MAX_REDIRECTS 32

/****************************************************************
 * G_spawn(char *command, ...)
 *
 * This is a more useful alternative to G_system(), which takes
 * the command's arguments as separate parameters.
 *
 ****************************************************************/

#ifdef __MINGW32__
int G_spawn(char *command, ...)
{
    G_fatal_error("G_spawn is not supported on Windows");    
    return -1;
}
#else
int G_spawn(char *command, ...)
{
	va_list va;
	char *args[MAX_ARGS];
	int num_args = 0;
	struct sigaction act, intr, quit;
	sigset_t block, oldmask;
	int status = -1;
	pid_t pid;

	args[0] = command;

	va_start(va, command);

	for (num_args = 1; num_args < MAX_ARGS; )
	{
		char *arg = va_arg(va, char *);
		if (!arg)
			break;
		args[num_args++] = arg;
	}

	va_end(va);

	if (num_args >= MAX_ARGS)
	{
		G_warning(_("too many arguments"));
		return -1;
	}

	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_RESTART;

	act.sa_handler = SIG_IGN;
	if (sigaction(SIGINT, &act, &intr) < 0)
		goto error_1;
	if (sigaction(SIGQUIT, &act, &quit) < 0)
		goto error_2;

	sigemptyset(&block);
	sigaddset(&block, SIGCHLD);
	if (sigprocmask(SIG_BLOCK, &block, &oldmask) < 0)
		goto error_3;

	pid = fork();

	if (pid < 0)
	{
		G_warning(_("unable to create a new process"));
		goto error_4;
	}

	if (pid == 0)
	{
		sigaction(SIGINT, &intr, NULL);
		sigaction(SIGQUIT, &quit, NULL);

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

error_4:
	sigprocmask(SIG_SETMASK, &oldmask, NULL);
error_3:
	sigaction(SIGQUIT, &quit, NULL);
error_2:
	sigaction(SIGINT, &intr, NULL);
error_1:
	return status;
}

/****************************************************************
 * G_spawn_ex(char *command, ...)
 *
 * This is a more advanced version of G_spawn().
 *
 ****************************************************************/

struct redirect
{
	int dst_fd;
	int src_fd;
	char *file;
	int mode;
};

struct signal
{
	int which;
	int action;
	int signum;
	int valid;
	struct sigaction old_act;
	sigset_t old_mask;
};

struct binding
{
	char *var;
	char *val;
};

static int undo_signals(struct signal *signals, int num_signals, int which)
{
	int error = 0;
	int i;

	for (i = num_signals-1; i >= 0; i--)
	{
		struct signal *s = &signals[i];

		if (s->which != which)
			continue;

		if (!s->valid)
			continue;

		switch (s->action)
		{
		case SSA_IGNORE:
		case SSA_DEFAULT:
			if (sigaction(s->signum, &s->old_act, NULL) < 0)
			{
				G_warning(_("G_spawn: unable to restore signal %d"), s->signum);
				error = 1;
			}
			break;
		case SSA_BLOCK:
		case SSA_UNBLOCK:
			if (sigprocmask(SIG_UNBLOCK, &s->old_mask, NULL) < 0)
			{
				G_warning(_("G_spawn: unable to restore signal %d"), s->signum);
				error = 1;
			}
			break;
		}
	}

	return !error;
}

static int do_signals(struct signal *signals, int num_signals, int which)
{
	struct sigaction act;
	sigset_t mask;
	int error = 0;
	int i;

	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_RESTART;

	for (i = 0; i < num_signals; i++)
	{
		struct signal *s = &signals[i];

		if (s->which != which)
			continue;

		switch (s->action)
		{
		case SSA_IGNORE:
			act.sa_handler = SIG_IGN;
			if (sigaction(s->signum, &act, &s->old_act) < 0)
			{
				G_warning(_("G_spawn: unable to reset signal %d"), s->signum);
				error = 1;
			}
			else
				s->valid = 1;
			break;
		case SSA_DEFAULT:
			act.sa_handler = SIG_DFL;
			if (sigaction(s->signum, &act, &s->old_act) < 0)
			{
				G_warning(_("G_spawn: unable to ignore signal %d"), s->signum);
				error = 1;
			}
			else
				s->valid = 1;
			break;
		case SSA_BLOCK:
			sigemptyset(&mask);
			sigaddset(&mask, s->signum);
			if (sigprocmask(SIG_BLOCK, &mask, &s->old_mask) < 0)
			{
				G_warning(_("G_spawn: unable to block signal %d"), s->signum);
				error = 1;
			}
			break;
		case SSA_UNBLOCK:
			sigemptyset(&mask);
			sigaddset(&mask, s->signum);
			if (sigprocmask(SIG_UNBLOCK, &mask, &s->old_mask) < 0)
			{
				G_warning(_("G_spawn: unable to unblock signal %d"), s->signum);
				error = 1;
			}
			else
				s->valid = 1;
			break;
		}
	}

	return !error;
}

static void do_redirects(struct redirect *redirects, int num_redirects)
{
	int i;

	for (i = 0; i < num_redirects; i++)
	{
		struct redirect *r = &redirects[i];

		if (r->file)
		{
			r->src_fd = open(r->file, r->mode, 0666);

			if (r->src_fd < 0)
			{
				G_warning(_("G_spawn: unable to open file %s"), r->file);
				_exit(127);
			}

			if (dup2(r->src_fd, r->dst_fd) < 0)
			{
				G_warning(_("G_spawn: unable to duplicate descriptor %d to %d"), r->src_fd, r->dst_fd);
				_exit(127);
			}

			close(r->src_fd);
		}
		else if (r->src_fd >= 0)
		{
			if (dup2(r->src_fd, r->dst_fd) < 0)
			{
				G_warning(_("G_spawn: unable to duplicate descriptor %d to %d"), r->src_fd, r->dst_fd);
				_exit(127);
			}
		}
		else
			close(r->dst_fd);
	}
}

static void do_bindings(struct binding *bindings, int num_bindings)
{
	int i;

	for (i = 0; i < num_bindings; i++)
	{
		struct binding *b = &bindings[i];
		char *str;

		str = G_malloc(strlen(b->var) + strlen(b->val) + 2);
		sprintf(str, "%s=%s", b->var, b->val);
		putenv(str);
	}
}

int G_spawn_ex(char *command, ...)
{
	char *args[MAX_ARGS];
	int num_args = 0;
	struct redirect redirects[MAX_REDIRECTS];
	int num_redirects = 0;
	struct signal signals[MAX_SIGNALS];
	int num_signals = 0;
	struct binding bindings[MAX_BINDINGS];
	int num_bindings = 0;
	int background = 0;
	char *directory = NULL;
	va_list va;
	char *var, *val;
	int status = -1;
	pid_t pid;

	args[num_args++] = command;

	va_start(va, command);

	for (;;)
	{
		char *arg = va_arg(va, char *);

		switch ((int) arg)
		{
		case 0:
			args[num_args++] = NULL;
			break;
		case ((int) SF_REDIRECT_FILE):
			redirects[num_redirects].dst_fd = va_arg(va, int);
			redirects[num_redirects].src_fd = -1;
			redirects[num_redirects].mode = va_arg(va, int);
			redirects[num_redirects].file = va_arg(va, char *);
			num_redirects++;
			break;
		case ((int) SF_REDIRECT_DESCRIPTOR):
			redirects[num_redirects].dst_fd = va_arg(va, int);
			redirects[num_redirects].src_fd = va_arg(va, int);
			redirects[num_redirects].file = NULL;
			num_redirects++;
			break;
		case ((int) SF_CLOSE_DESCRIPTOR):
			redirects[num_redirects].dst_fd = va_arg(va, int);
			redirects[num_redirects].src_fd = -1;
			redirects[num_redirects].file = NULL;
			num_redirects++;
			break;
		case ((int) SF_SIGNAL):
			signals[num_signals].which = va_arg(va, int);
			signals[num_signals].action = va_arg(va, int);
			signals[num_signals].signum = va_arg(va, int);
			signals[num_signals].valid = 0;;
			num_signals++;
			break;
		case ((int) SF_VARIABLE):
			var = va_arg(va, char *);
			val = getenv(var);
			args[num_args++] = val ? val : "";
			break;
		case ((int) SF_BINDING):
			bindings[num_bindings].var = va_arg(va, char *);
			bindings[num_bindings].val = va_arg(va, char *);
			num_bindings++;
			break;
		case ((int) SF_BACKGROUND):
			background = 1;
			break;
		case ((int) SF_DIRECTORY):
			directory = va_arg(va, char *);
			break;
		default:
			args[num_args++] = arg;
			break;
		}

		if (!arg)
			break;
	}

	va_end(va);

	if (!do_signals(signals, num_signals, SST_PRE))
		goto error_1;

	pid = fork();
	if (pid < 0)
	{
		G_warning(_("unable to create a new process"));
		goto error_2;
	}

	if (pid == 0)
	{
		if (!undo_signals(signals, num_signals, SST_PRE))
			_exit(127);

		if (!do_signals(signals, num_signals, SST_CHILD))
			_exit(127);

		if (directory)
			if (chdir(directory) < 0)
			{
				G_warning(_("unable to change directory to %s"), directory);
				_exit(127);
			}

		do_redirects(redirects, num_redirects);
		do_bindings(bindings, num_bindings);

		execvp(command, args);
		G_warning(_("unable to execute command"));
		_exit(127);
	}

	do_signals(signals, num_signals, SST_POST);

	if (background)
		status = (int) pid;
	else
	{
		pid_t n;

		do n = waitpid(pid, &status, 0);
		while (n == (pid_t) -1 && errno == EINTR);

		if (n != pid)
			status = -1;
	}

	undo_signals(signals, num_signals, SST_POST);
error_2:
	undo_signals(signals, num_signals, SST_PRE);
error_1:

	return status;
}
#endif /*#ifdef __MINGW32__*/
