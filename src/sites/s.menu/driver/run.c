#include "gis.h"
#include "site.h"
#include "run.h"

run (site_list, pgm, action, clipped, with_window)

	SITE_LIST *site_list;
	char *pgm;
{
	static char *sites_file = 0;
	char command[1024];
	int stat;

	if (!sites_file)
		sites_file = G_tempfile () ;

	if (!put_site_list (site_list, sites_file, clipped, with_window))
		return -1;

	if (action == RUN_PIPED)
		sprintf (command, "cd; cat %s | %s", sites_file, pgm);
	else
		sprintf (command, "cd; %s %s", pgm, sites_file);

	stat = execute (command);

	if (action == RUN_MODIFY)
	{
		if (!get_site_list (site_list, sites_file))
			return -1;
	}

	unlink (sites_file);
	return stat;
}
