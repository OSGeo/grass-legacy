#include "globals.h"

sigalarm(n)
{
	signal (n,sigalarm);
	signalflag.alarm = n;
}
