#include "glob.h"
yes (prompt, with_default)
{
    char ok[40];

    while(1)
    {
	fprintf (stderr, "%s ", prompt);
	if (with_default)
		fprintf (stderr, "[y] ");
	else
		fprintf (stderr, "yes or no? ");
	if (!G_gets(ok)) continue;
	G_strip (ok);
	if (with_default && *ok == 0)
		return 1;
	if (*ok == 'y' || *ok == 'Y') return 1;
	if (*ok == 'n' || *ok == 'N') return 0;
    }
}
