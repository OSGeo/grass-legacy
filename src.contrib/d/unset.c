#include <stdio.h>
#include "level.h"

static char *HELP =
"\
\n\
    c(ell)\n\
    l(abels)\n\
    o(verlay)\n\
    s(ites)\n\
    v(ector)\n\
\n\
    a(ll)\n\
\n";

unset (argc, argv) char **argv;
{
    int any;
    char buf[100];

    any = 0;
    if (argc == 0)
    {
	if (!isatty(0)) return 0;
	printf ("Select from the following\n%s\n", HELP);
	while (1)
	{
	    printf ("enter type: ");
	    if (!G_gets(buf)) continue;
	    G_strip (buf);
	    if (*buf == 0) break;

	    switch (*buf)
	    {
	    case 'a': erase(); return 1;
	    case 'c': set_cell(NULL); break;
	    case 'o': unset_level(OVERLAY_LEVEL); break;
	    case 'v': unset_level(VECTOR_LEVEL); break;
	    case 's': unset_level(SITES_LEVEL); break;
	    case 'l': unset_level(LABEL_LEVEL);
		      break;
	    default:
		printf ("Select from the following\n%s\n", HELP);
		continue; /* while */
	    }
	    printf ("ok\n");
	    any = 1;
	}
    }
    else while (argc-- > 0)
    {
	char *a;

	for (a = *argv++; *a; a++)
	{
	    switch (*a)
	    {
	    case 'a': erase(); return 1;
	    case 'c': set_cell(NULL); break;
	    case 'o': unset_level(OVERLAY_LEVEL); break;
	    case 'v': unset_level(VECTOR_LEVEL); break;
	    case 's': unset_level(SITES_LEVEL); break;
	    case 'l': unset_level(LABEL_LEVEL);
		      break;
	    default:  printf ("%c - type unknown. ignored\n", *a);
		      printf ("Valid types:\n%s\n", HELP);
		      continue; /* for */
	    }
	    any = 1;
	}
    }
    if (any)
	run_script (0,0);
    return any ? 1 : 0;
}


unset_level (level)
{
    char **list;
    int count;
    int n, lv;
    int any;

    get_key ("script", &list, &count);
    delete_key ("script");

    any = 0;
    for (n = 0; n < count; n++)
    {
	if (sscanf (list[n], "%d", &lv) != 1) continue;
	if (lv == level) any = 1;
	else append_key ("script", list[n]);
    }
}
