#include "gis.h"
#include "config.h"
#include <stdio.h>

/*
 * process_new
 *
 */

process_new (out, file)
FILE *out;
char *file;
{
	char *tmp_fname;
	FILE *tmp;
	char response[5];
	int i, ok=0;

	while (1)
	{
		/* initialize variables */
		for (i = 0; i < TEXTLINES; i++)
			config.text[i][0] = 0;
		config.xoffset[0] = config.yoffset[0] = 0;
		strcpy (config.skip,"no");

		/* ask user to indicate location of next label */
		if (get_location(config.east,config.north))
			return(1);

		/* loop until user says a label looks ok or quits */
		ok = 0;
		while (!ok)
		{
			/* setup VASK screen with label parameters for this label */
			strcpy (reset_loc,"no");
			setup (file,1);

			/* present the VASK screen */
			if(!modify())
				return 0;

			ok = check_responses();

			if (ok=1)
			{
				if (!strcmp("Y",reset_loc)   || !strcmp("yes",reset_loc) ||
				    !strcmp("YES",reset_loc) || !strcmp("y",reset_loc) )
				{
					/* allow user to choose new location for label */
					get_location(config.east,config.north);
					ok = 0;
				}
				else 
				{
					tmp_fname = G_tempfile();
					tmp = fopen(tmp_fname,"w");   /* open temp file */

					update(tmp);                  /* write label info to it */

					config.count++;
					fclose(tmp);
					if(strcmp(config.skip,"YES")==0||strcmp(config.skip,"yes")==0)
						ok=1;
					else
					{
						/* plot label for users approval */
						ok = show_a_label(tmp_fname);
					}
				}
			}
		}

		/* update the label file */
		if(!update(out))
			return 1;
	}
}
