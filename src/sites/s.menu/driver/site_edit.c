#include "site.h"
#include "run.h"

site_edit (site_list)

	SITE_LIST *site_list;
{
	char buf[100];
	char pgm[100];

	do
	printf("which unix editor would you like to use? ");
	while (!G_gets(buf)) ;

	if (sscanf (buf, "%s", pgm) != 1)
		return 0;

	printf("starting %s ...\n", pgm);

	return run (site_list, pgm, RUN_MODIFY, 0, 1);
}
