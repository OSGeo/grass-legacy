#include "externs.h"

sigint(n)
{
	signal (n,sigint);
	signalflag.interrupt = n;
}
