#include "gis.h"
#include "report.h"

static char *quad_explanation[] = {

"Please set the site quad mask size.",
"This is the number of cells away from the site to be included in the analysis",
"eg, enter",
"  0 if you just want to include the site itself",
"  1 if you want to include the 8 cells surrounding the site",
"  2 if you want to include both the above 8 cells and the 14 cells",
"    surrounding those 8, etc.",
"",
"enter <stop> if you want to stop",

0};

int 
ask_quad (int *quadsize)
{
	char **s;
	char buf[300];

/*
 * ask user for quad mask size. This is the number of cells away
 * from the actual site to include in the analysis
 */
	new_report_screen();

	for (s = quad_explanation; *s; s++)
		fprintf (stdout,"%s\n", *s);

	while (1)
	{
		fprintf (stdout,"\nsite quad mask size> ");
		if (!G_gets(buf)) continue;
		fprintf (stdout,"<<%s>>\n", buf);
		if (strcmp(buf,"stop") == 0)
			return 0;
		if(!scan_int(buf, quadsize))
			continue;
		if (*quadsize >= 0)
			break;
		fprintf (stdout,"can't be negative\n");
	}

	return 1;
}
