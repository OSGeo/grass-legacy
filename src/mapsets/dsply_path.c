/* %W% %G% */
#include "gis.h"
display_mapset_path()
{
	printf("Your mapset search list:\n");
	_display_mapset_path ();
	printf ("\n");
}

_display_mapset_path()
{
	int n;
	char *name;
	int len;
	int nleft;

	nleft = 78;
	for (n = 0; name = G__mapset_name(n); n++)
	{
		len = strlen (name);
		if (len > nleft)
		{
			printf("\n");
			nleft = 78;
		}
		printf("%s ", name);
		nleft -= (len + 1);
	}
	printf("\n");
}
