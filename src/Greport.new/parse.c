#include "global.h"

parse_command_line (argc, argv)
    char *argv[];
{
    int i;

    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')
	    parse_option(argv[i]);
	else
	    parse_layer(argv[i]);
    }
    if (nlayers <= 0)
	usage();
}

parse_option (s)
    char *s;
{
    int x;
    s++;

    switch (*s++)
    {
    case 'u':
	if (!strcmp (s, "mi"))
	    x = SQ_MILES;
	else if (!strcmp (s, "me"))
	    x = SQ_METERS;
	else if (!strcmp (s, "k"))
	    x = SQ_KILOMETERS;
	else if (!strcmp (s, "a"))
	    x = ACRES;
	else if (!strcmp (s, "h"))
	    x = HECTARES;
	else if (!strcmp (s, "c"))
	    x = CELL_COUNTS;
	else if (!strcmp (s, "p"))
	    x = PERCENT_COVER;
	else
	    usage();
	if (nunits >= MAX_UNITS)
	{
	    fprintf (stderr, "\nERROR: %s: only %d unit%s allowed\n",
		G_program_name(), MAX_UNITS, MAX_UNITS==1?"":"s");
	    exit(1);
	}
	unit[nunits].type  = x;
	nunits++;
	break;

    case 'l':
	if (sscanf (s, "%ld", &page_length) != 1 || page_length < 0)
	    usage();
	break;
    case 'w':
	if (sscanf (s, "%ld", &page_width) != 1 || page_width < 1)
	    usage();
	break;
    case 'f':
	if (*s)
	    usage();
	use_formfeed = 1;
	break;
    case 'h':
	if (*s)
	    usage();
	with_headers = 0;
	break;
    case 'n':
	if (*s)
	    usage();
	masking = 0;
	break;
    default:
	usage();
    }
}

parse_layer(s)
    char *s;
{
    char msg[100];
    char name[200];
    char *mapset;
    int n;

    strcpy (name, s);
    mapset = G_find_cell2 (name, "");

    if (mapset == NULL)
    {
	sprintf (msg, "%s: <%s> cell file not found\n", G_program_name(), s);
	G_fatal_error (msg);
	exit(1);
    }

    n = nlayers++ ;
    layers = (LAYER *)G_realloc(layers, nlayers * sizeof(LAYER));
    layers[n].name = G_store (name);
    layers[n].mapset = mapset;
    G_read_cats (name, mapset, &layers[n].labels);
}

usage()
{
    fprintf (stderr, "Usage:\n\n");
    fprintf (stderr, "%s [options] layer(s)\n", G_program_name());

    fprintf (stderr, "\n");
    fprintf (stderr, "Options:\n\n");
    fprintf (stderr, "  units:\n");
    fprintf (stderr, "   -ua  acres           -uk   square kilometers\n");
    fprintf (stderr, "   -uh  hectares        -ume  square meters\n");
    fprintf (stderr, "   -uc  cell counts     -umi  square miles\n");
    fprintf (stderr, "   -up  percent cover\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "  page control:\n");
    fprintf (stderr, "   -h   header on first page only\n");
    fprintf (stderr, "   -f#  # use formfeeds after each page\n");
    fprintf (stderr, "   -l#  # lines per page (default %d)\n",
	DEFAULT_PAGE_LENGTH);
    fprintf (stderr, "   -w#  # characters per line (default %d)\n",
	DEFAULT_LINE_WIDTH);
    exit(1);
}
