#include "global.h"

get_stats()
{
    char buf[1024];
    char *tempfile;
    int i,nl,ns;
    FILE *fd;
    char **tokens;
    char **G_tokenize();
    int cmp();

    tempfile = G_tempfile();
    stats_file = G_tempfile();

    strcpy (buf, "Gstats -C");
    if (!masking) strcat (buf, "n");

    for (i = 0; i < nlayers; i++)
    {
	strcat (buf, " '");
	strcat (buf, layers[i].name);
	strcat (buf, " in ");
	strcat (buf, layers[i].mapset);
	strcat (buf, "'");
    }

    strcat (buf, " > ");
    strcat (buf, tempfile);
    if(system(buf))
    {
	unlink (tempfile);
	exit(1);
    }

    fd = fopen (tempfile, "r");

    if (fd == NULL)
    {
	unlink (tempfile);
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
    unlink (tempfile);
}

static
die()
{
    G_fatal_error ("OOPS - problem reading Gstats output\n");
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
