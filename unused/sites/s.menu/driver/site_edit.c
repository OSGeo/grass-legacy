#include "gis.h"
#include "site.h"
#include "run.h"

int 
site_edit (SITE_LIST *site_list)
{
	char buf[100];
	char pgm[100];

	do
	fprintf (stdout,"which unix editor would you like to use? ");
	while (!G_gets(buf)) ;

	if (sscanf (buf, "%s", pgm) != 1)
		return 0;

	fprintf (stdout,"starting %s ...\n", pgm);

	return run (site_list, pgm, RUN_MODIFY, 0, 1);
}
