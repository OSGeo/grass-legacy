#include "gis.h"
#include "site.h"
#include "local_proto.h"

static char *quad_explanation[] = {

"Please set the site quad size.",
"This is the number of cells away from the site to be included in the site",
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
    int print_prompt;

/*
 * ask user for quad size. This is the number of cells away
 * from the actual site to include in the site
 */
    new_screen();


    print_prompt = 1;
    while (1)
    {
	if (print_prompt)
	{
	    for (s = quad_explanation; *s; s++)
		fprintf (stdout,"%s\n", *s);
	}
	fprintf (stdout,"\nsite quad size> ");
	print_prompt = 1;
	if (!G_gets(buf)) continue;
	print_prompt = 0;
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
