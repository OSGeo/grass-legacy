#include "global.h"

get_stats()
{
    char buf[1024];
    int i,nl,ns;
    FILE *fd;
    char **tokens;
    char **G_tokenize();
    int cmp();

    if (stats_flag == EVERYTHING)
	stats_file = G_tempfile();

    if (stats_flag != REPORT_ONLY)
    {
	strcpy (buf, "r.stats -ac");
	if (!masking) strcat (buf, "m");
	if (!verbose) strcat (buf, "q");
	if (z_option) strcat (buf, "z");
	strcat (buf, " fs=: 'input=");

	for (i = 0; i < nlayers; i++)
	{
	    if (i) strcat (buf, ",");
	    strcat (buf, G_fully_qualified_name(layers[i].name,layers[i].mapset));
	}
	strcat (buf, "'");

	strcat (buf, " > ");
	strcat (buf, stats_file);
/*	G_fatal_error(buf);*/
	if(system(buf))
	{
	    if (stats_flag == EVERYTHING)
		unlink (stats_file);
	    exit(1);
	}
    }
    if (stats_flag == STATS_ONLY) return;

    fd = fopen (stats_file, "r");

    if (fd == NULL)
    {
	if (stats_flag == EVERYTHING)
	    unlink (stats_file);
	sprintf (buf, "%s: unable to open result file <%s>\n",	
		G_program_name(), stats_file);
	G_fatal_error (buf);
    }
    while (G_getl(buf, sizeof buf, fd))
    {
	tokens = G_tokenize (buf, ":");
	i = 0;
	ns = nstats++;
	Gstats = (GSTATS *)G_realloc (Gstats, nstats * sizeof(GSTATS));
	Gstats[ns].cats = (long *)G_calloc (nlayers, sizeof(long));
	for (nl = 0; nl < nlayers; nl++)
	{
	    if (sscanf (tokens[i++], "%ld", &Gstats[ns].cats[nl]) != 1)
		die();
	}
	if (sscanf (tokens[i++], "%lf", &Gstats[ns].area) != 1)
	    die();
	if (sscanf (tokens[i++], "%ld", &Gstats[ns].count) != 1)
	    die();
	G_free_tokens (tokens);
    }
    fclose (fd);
    if (stats_flag == EVERYTHING)
	unlink (stats_file);
}

static
die()
{
    if (stats_flag == EVERYTHING)
	unlink (stats_file);
    G_fatal_error ("OOPS - problem reading r.stats output\n");
}

static
cmp (a, b)
    GSTATS *a, *b;
{
    int i;
    for (i = 0; i < nlayers; i++)
    {
	if(a->cats[i] < b->cats[i]) return -1;
	if(a->cats[i] > b->cats[i]) return 1;
    }
    return 0;
}
