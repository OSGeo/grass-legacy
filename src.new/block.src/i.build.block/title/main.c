#include "dba_imagery.h"

main (argc, argv) char *argv[];
{
    char *block;
    char title[79];
    char line[80];
    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s block\n", argv[0]);
	exit(1);
    }

    block = argv[1];

    G_gisinit (argv[0]);
    if (!I_find_block(block))
    {
	fprintf (stderr, "%s: block [%s] not found\n", argv[0], block);
	exit(1);
    }

    sprintf (title, "BLOCK: %s", block);
    I_location_info (line, title);
    I_get_block_title (block, title, sizeof title);
    V_clear();
    V_line (1, line);
    V_line (4, "TITLE:");
    V_ques (title, 's', 5, 0, sizeof (title)-1);
    while (1)
    {
	V_intrpt_ok();
	if (!V_call())
	    exit(0);
	G_strip (title);
	printf ("%s\n", title);
	printf ("---------------------------------------\n");
	if (G_yes("Look ok? ", 0))
	    break;
    }
    I_put_block_title (block, title);
    exit(0);
}
