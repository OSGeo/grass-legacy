#include "globals.h"
#include <signal.h>

set_signals()
{
/*	int ctrlz(); */
	int quit();
	int sigint();


/* set the ctrlz catch 
	signal (SIGTSTP, ctrlz);
*/
	signal (SIGTSTP, SIG_IGN); /* ignore ctrl-Z */
/* set other signal catches */

	signalflag.interrupt = 0;
	signal (SIGINT, sigint);

	signal (SIGTERM, quit);
}
