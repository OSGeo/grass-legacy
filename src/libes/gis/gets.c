#include <stdio.h>
#include <signal.h>
/**********************************************************
 * G_gets (buf)
 *    char *buf      buffer to receive data
 *
 *  does a gets() from stdin. exits upon EOF.
 *  if stdin is a tty (ie, not a pipe or redirected)
 *  then ctrl-z is detected
 *
 * returns
 *  1 read ok
 *  0 ctrl-z entered. calling routine should re-print a prompt
 *    and call G_gets() again
 *
 * note: This is very useful for allowing a program to 
 *       reprompt after the program is restarted after
 *       being stopped wint a ctrl-Z.
 *
 * sample use:
 *   do {
 *      printf("Enter some input:  ") ;
 *   } while ( ! G_gets(buff) )  ;
 *
 *   If the user suspends the process at this prompt G_gets will return
 *   "0" causing the reprompting.
 ***********************************************************/

static int ctrlz = 0;

G_gets (buf)
	char *buf;
{
	void (*sigtstp)();
	void catch_ctrlz();
	int tty;
	int eof;

	ctrlz = 0;
#ifdef SIGTSTP
	if (tty = isatty(0))
	{
		sigtstp = (int (*)()) signal (SIGTSTP, catch_ctrlz);
		if (sigtstp != (int (*)()) SIG_DFL)
		    signal (SIGTSTP, sigtstp);
	}
#endif
	eof = !gets(buf);
#ifdef SIGTSTP
	if (tty)
		signal (SIGTSTP, sigtstp);
#endif
	if (!eof)
		return 1;
	if (ctrlz)
		return 0;
	printf("EOF\n");
	exit(1);
}

static void catch_ctrlz(n)
{
	void (*sigint)();
	void catch_int();

/* having caught ctrlz - effect a ctrl-z using kill */
	ctrlz = 1;
	signal (n, SIG_DFL);
	kill (0, n);

/* for berkley systems, ctrlz will not cause eof on read */
	sigint = signal (SIGINT, catch_int);
	kill (getpid(), SIGINT);
	signal (SIGINT, sigint);
}

static void catch_int(n)
{
}
