#include "global.h"

parse_command_line (argc, argv)
char *argv[];
{
	char pl_desc[256];
	char pw_desc[256];
	int i;
	struct
	    {
		struct Option *cell;
		struct Option *units;
		struct Option *pl;    /* page length */
		struct Option *pw;    /* page width */
		struct Option *outfile;
	} parms;
	struct
	    {
		struct Flag *f;
		struct Flag *m;
		struct Flag *h;
		struct Flag *q;
		struct Flag *e;
		struct Flag *z;
	} flags;

	parms.cell = G_define_option();
	parms.cell->key    = "map";
	parms.cell->type   = TYPE_STRING ;
	parms.cell->required = YES ;
	parms.cell->multiple = YES ;
	parms.cell->gisprompt  = "old,cell,raster" ;
	parms.cell->description = "raster map(s) to report on";

	parms.units = G_define_option();
	parms.units->key   = "units";
	parms.units->type   = TYPE_STRING ;
	parms.units->required = NO ;
	parms.units->multiple = YES ;
	parms.units->description =
	    "mi(les),me(ters),k(ilometers),a(cres),h(ectacres),c(ell_counts),p(ercent_cover)";

	parms.pl = G_define_option();
	parms.pl->key = "pl";
	parms.pl->type = TYPE_INTEGER;
	parms.pl->required = NO ;
	sprintf (pl_desc, "page length (default: %d lines)", DEFAULT_PAGE_LENGTH);
	parms.pl->description = pl_desc;

	parms.pw = G_define_option();
	parms.pw->key = "pw";
	parms.pw->type = TYPE_INTEGER;
	parms.pw->required = NO ;
	sprintf (pw_desc, "page width (default: %d characters)", DEFAULT_PAGE_WIDTH);
	parms.pw->description = pw_desc;

	parms.outfile = G_define_option();
	parms.outfile->key = "output";
	parms.outfile->type = TYPE_STRING;
	parms.outfile->required = NO ;
	parms.outfile->description = "name of an output file to hold the report";

	flags.h = G_define_flag();
	flags.h->key = 'h';
	flags.h->description = "suppress page headers";

	flags.m = G_define_flag();
	flags.m->key = 'm';
	flags.m->description = "report zero values due to mask";

	flags.f = G_define_flag();
	flags.f->key = 'f';
	flags.f->description = "use formfeeds between pages";

	flags.q = G_define_flag();
	flags.q->key = 'q';
	flags.q->description = "quiet";

	flags.e = G_define_flag();
	flags.e->key = 'e';
	flags.e->description = "scientific format";

	flags.z = G_define_flag();
	flags.z->key = 'z';
	flags.z->description = "filter out zero category data";
/* hidden feature.
 * if first arg is >file just run r.stats into this file and quit
 * if first arg is <file, run report from stats in file
 * (this feature is for the interactive version of this program -
 *  to get more than one report without re-running r.stats)
 */
	stats_flag = EVERYTHING;
	if (argc > 1)
	{
		if (argv[1][0] == '<' || argv[1][0] == '>')
		{
			stats_file = argv[1]+1;
			if (argv[1][0] == '<')
				stats_flag = REPORT_ONLY;
			else
			{
				unlink (stats_file);
				stats_flag = STATS_ONLY;
			}
			argc--;
			argv++;
		}
	}

	if (G_parser(argc,argv))
		exit(0);

	use_formfeed = flags.f->answer;
	with_headers = !flags.h->answer;
	masking      = !flags.m->answer;
	verbose      = !flags.q->answer;
	e_format     = flags.e->answer;
	z_option     = flags.z->answer;

	for (i = 0; parms.cell->answers[i]; i++)
		parse_layer (parms.cell->answers[i]);
	if (parms.units->answers)
		for (i = 0; parms.units->answers[i]; i++)
			parse_units (parms.units->answers[i]);

	if (parms.pl->answer)
	{
		if (sscanf (parms.pl->answer, "%ld", &page_length) != 1 || page_length < 0)
		{
			fprintf (stderr, "Illegal page length\n");
			G_usage();
			exit(1);
		}
	}

	if (parms.pw->answer)
	{
		if (sscanf (parms.pw->answer, "%ld", &page_width) != 1 || page_width < 1)
		{
			fprintf (stderr, "Illegal page width\n");
			G_usage();
			exit(1);
		}
	}
	if (parms.outfile->answer)
	{	
		if (freopen (parms.outfile->answer, "w", stdout) == NULL)
		{
			perror (parms.outfile->answer);
			exit(1);
		}
	}
}

parse_units (s)
char *s;
{
	int x;

	if (match (s, "miles",2))
		x = SQ_MILES;
	else if (match (s, "meters",2))
		x = SQ_METERS;
	else if (match (s, "kilometers",1))
		x = SQ_KILOMETERS;
	else if (match (s, "acres",1))
		x = ACRES;
	else if (match (s, "hectares",1))
		x = HECTARES;
	else if (match (s, "cell_counts",1))
		x = CELL_COUNTS;
	else if (match (s, "counts",1))
		x = CELL_COUNTS;
	else if (match (s, "percent_cover",1))
		x = PERCENT_COVER;
	else
	{
		G_usage();
		exit(1);
	}
	if (nunits >= MAX_UNITS)
	{
		fprintf (stderr, "\nERROR: %s: only %d unit%s allowed\n",
		    G_program_name(), MAX_UNITS, MAX_UNITS==1?"":"s");
		exit(1);
	}
	unit[nunits].type  = x;
	nunits++;
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
		sprintf (msg, "%s: <%s> raster map not found\n", G_program_name(), s);
		G_fatal_error (msg);
		exit(1);
	}

	n = nlayers++ ;
	layers = (LAYER *)G_realloc(layers, nlayers * sizeof(LAYER));
	layers[n].name = G_store (name);
	layers[n].mapset = mapset;
	G_read_cats (name, mapset, &layers[n].labels);
}

match (s, key, min)
char *s, *key;
{
	int len;

	len = strlen (s);
	if (len < min) return 0;
	return strncmp (s, key, len) == 0;
}
