/*  @(#)sigint.c	2.1  6/26/87  */
#include "externs.h"

sigint(n)
{
	signal (n,sigint);
	signalflag.interrupt = n;
}
