/*  @(#)set_signals.c	1.1  5/4/87  */
#include "externs.h"
#include <signal.h>

set_signals()
{
	int sigint();

/* set INT signal catch */
	signalflag.interrupt = 0;
	signal (SIGINT, sigint);
}
