#include "gis.h"
#include "site.h"
#include "dir.h"
get_site(site_list, Quiet)

	SITE_LIST *site_list;
	int Quiet;

{
	char *mapset;
	int stat;
	FILE *fd;

	stat = 0;
	/*printf("Sitelist= %s\n", site_list->name);*/
	if (mapset = G_find_file2 (SITE_DIR,site_list->name,""))
	{
		fd = G_fopen_old (SITE_DIR, site_list->name, mapset);
		if (!fd)
		{
			printf("unable to open site list %s\n", site_list->name);
			return -1;
		}
		if (!Quiet)
		{
	    	    announce ("reading site list <");
		    announce (site_list->name);
		    announce ("> ...");
		}
		if(!read_site_list (site_list, fd))
			stat = -1;
 		fclose (fd);
  		announce ("\n");
	}
	return(stat);
}
