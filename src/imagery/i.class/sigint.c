#include "globals.h"

sigint(n)
{
	signal (n,sigint);
	signalflag.interrupt = n;
}
