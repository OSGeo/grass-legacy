#include "site.h"
#include "run.h"

unix_cmd (pgm, site_list)

	char *pgm;
	SITE_LIST *site_list;
{
	char temp[3];

	if (sscanf (pgm, "%1s", temp) != 1)
		return 0;

	printf("%s\n", pgm);
	run (site_list, pgm, RUN_PIPED, 0, 0);
}
