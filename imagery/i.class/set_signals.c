#include "globals.h"
#include <signal.h>

void quit();
void sigint();

int set_signals (void)
{
/* set the ctrlz catch 
	signal (SIGTSTP, ctrlz);
*/
	signal (SIGTSTP, SIG_IGN); /* ignore ctrl-Z */
/* set other signal catches */

	signalflag.interrupt = 0;
	signal (SIGINT, sigint);

	signal (SIGTERM, quit);

	return 0;
}
