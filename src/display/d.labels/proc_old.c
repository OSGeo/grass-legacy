#include "gis.h"
#include <stdio.h>
#include "config.h"

process_old (in, out, file)
FILE *in, *out;
char *file;
{
	char *tmp_fname;
	FILE *tmp;
	int ok = 0;
	int first=1;

	if (!in)
		return 1;

	while (gather(in))
	{
		ok = 0;
		while (!ok)
		{
			/* get ready to plot label */
			tmp_fname = G_tempfile();
			tmp = fopen(tmp_fname,"w");           /* open temp file */
			update(tmp);                          /* write label info to it */
			config.count++;
			fclose(tmp);
			printf("\nLabel #%d\n\n",config.count+1);
			if ((strcmp(config.skip,"YES")==0  ||
			    strcmp(config.skip,"yes")==0) &&
			    !first)
				ok=1;
			else if (!(ok=show_a_label(tmp_fname))) /* show it, ask if ok */
			{
				first=0;
				strcpy (reset_loc,"no");           /* not ok */
				setup (file,0);
				modify() ;
				/*
				if(!modify())
					return 0;
				*/
				ok = check_responses();
				if (ok)
				{
					if (!strcmp("Y",reset_loc)   || !strcmp("yes",reset_loc) ||
					    !strcmp("YES",reset_loc) || !strcmp("y",reset_loc) )
					{
						/* allow user to choose new location for label */
						get_location(config.east,config.north);
						ok = 0;
					}
				}
			}
		}
		update(out);
	}
	return 1;
}
