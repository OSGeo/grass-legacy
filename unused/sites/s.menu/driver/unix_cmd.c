#include <stdio.h>
#include "site.h"
#include "run.h"

int unix_cmd (char *pgm, SITE_LIST *site_list)
{
	char temp[3];

	if (sscanf (pgm, "%1s", temp) != 1)
		return 0;

	fprintf (stdout,"%s\n", pgm);
	run (site_list, pgm, RUN_PIPED, 0, 0);

	return 0;
}
