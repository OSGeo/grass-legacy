#include <signal.h>
#include "globals.h"

void sigint (int n)
{
	signal (n,sigint);
	signalflag.interrupt = n;
}
