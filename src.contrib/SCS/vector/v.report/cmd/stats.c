#include "global.h"

get_stats()
{
    char buf[1024], atype[5];
    int i,nl,ns, nalloc;
    FILE *fd;
    char **tokens;
    char **G_tokenize();
    int cmp();

    if (stats_flag == EVERYTHING)
	stats_file = G_tempfile();

    if (stats_flag != REPORT_ONLY)
    {
	for (i = 0; i < nlayers; i++)
	  if (do_v_stats(verbose,i,stats_file) < 0) exit(-1);
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
    type = 0;

    while (G_getl(buf, sizeof buf, fd))
    {
	if (type == 0)
	   {
	   sscanf(buf,"%s %d",atype, &nalloc);
	   if (*atype == 'a') type = 1;
	   if (*atype == 'l') type = 2;
	   if (*atype == 's') type = 3;
	   if (type == 0)
	      {
	      fprintf(stderr,"Unknown stats data type\n");
	      die();
	      }
           Gstats = (GSTATS *) G_calloc (nalloc, sizeof(GSTATS));
           G_getl(buf, sizeof buf, fd);
           }

  	tokens = G_tokenize (buf, ":");
	i = 0;
	ns = nstats++;
	if (nstats > nalloc)
	   {
	   nalloc = nalloc + 256;
	   Gstats = (GSTATS *)G_realloc (Gstats, nalloc * sizeof(GSTATS));
	   }
	Gstats[ns].cats = (long *)G_calloc (nlayers, sizeof(long));
	for (nl = 0; nl < nlayers; nl++)
	{
	    if (sscanf (tokens[i++], "%ld", &Gstats[ns].cats[nl]) != 1)
		die();
	}

	if (sscanf (tokens[i++], "%ld", &Gstats[ns].count) != 1)
	    die();
	if (type == 1)
        {
	   if (sscanf (tokens[i++], "%lf", &Gstats[ns].area) != 1)
	       die();
        }
	else
        {
	   if (sscanf (tokens[i++], "%lf", &Gstats[ns].length) != 1)
	       die();
        }
	G_free_tokens (tokens);
/*if(type == 1) fprintf(stderr,"Gstats[%d]: cat= %d, cnt= %d, area= %10.3e\n",ns,Gstats[ns].cats[0],Gstats[ns].count,Gstats[ns].area);
else fprintf(stderr,"Gstats[%d]: cat= %d, cnt= %d, leng= %10.3e\n",ns,Gstats[ns].cats[0],Gstats[ns].count,Gstats[ns].length);*/
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
    G_fatal_error (" - problem reading data stats output\n");
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
