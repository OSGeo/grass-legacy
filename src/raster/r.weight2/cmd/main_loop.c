#include "gis.h"
#define MAXFILS 6

char tmp_names[MAXFILS+1][50];
static char *old_mapset, old_name[256], tmpbuf1[256], tmpbuf2[256], buf[512];
static int do_end ();
char *strip_quotes ();
static char *tempcell ();
static char *acts[] = {
	NULL,
	" + ",
	" * "
    };


main_loop (result, tmpfp, action, title)
    char *result;
    FILE *tmpfp;
    int action;
    char *title;
{
    int got_old, got_rule;
    int istty, stop;
    int isfile;
    int filecnt;
    char prompt[50];
    int i;

    *title = 0;

    for (i = 0 ; i <= MAXFILS ; i++)
	tmp_names[i][0] = '\0';
    istty = isatty (0);
    got_old = 0;
    got_rule = 0;
    stop = 0;
    filecnt = 0;
    *prompt = '\0';
    while (input(buf, prompt))
    {
	if (!strlen (buf))
	    continue;
	/* if too many files */
	if (stop)
	    continue;

	/* if new file name */
	isfile = 0;
	if (sscanf (buf, " %s %s", tmpbuf1, tmpbuf2) == 1)
	    if (!numeric (tmpbuf1))  isfile = 1;
	else
	    if (!numeric (tmpbuf1) || strcmp (tmpbuf2, "in") == 0)
		isfile = 1;
	if (isfile)
	{
	    G_squeeze (buf);
	    strip_quotes (buf);
	    G_squeeze (buf);
	    if (strcmp(buf,"list")==0)
	    {
		G_list_element ("cell", "raster", "", (int (*)()) NULL);
		continue;
	    }

	    if (filecnt == MAXFILS)
	    {
		stop = 1;
		fprintf (stderr, "Maximum of %d files exceeded\n", MAXFILS);
		if (istty)
		{
		    *prompt = '\0';
		    fprintf  (stderr, "  -> 'end' or 'exit'\n");
		    continue;
		}
		else
		    exit (-1);
	    }

	/* we have old file and just got another file name */
	/* so clean up old and start new */
	    do_end (tmpfp, filecnt, &got_old, &got_rule);

	    old_mapset = G_find_cell (old_name, "");
	    if (old_mapset == NULL)
	    {
		*prompt = '\0';
		if (istty)
		{
		    fprintf (stderr,"Map layer '%s' not found\n", old_name);
		    got_old = 0;
		    continue;
		}
		else
		{
		    sprintf (buf, "%s - not found", old_name);
		    G_fatal_error (buf);
		    exit(1);
		}
	    }
	    got_old = 1;
	    filecnt++;
	    got_rule = 0;
	    strcpy (prompt, old_name);
	    if (istty) printf ("\n");
	    continue;
	}

	if (!got_old)
	    if (istty)
	    {
		fprintf (stderr, "No data layer defined");
		fprintf (stderr,  "  Format:   layer=cellfile\n");
		continue;
	    }
	    else
		G_fatal_error ("No data layer defined");
	else
	{
	    /* have old file name, just got a rule, but havent dealt w/ it */
	    /* yet.  so write out  r.reclass command, then we will start    */
	    /* filling in the input to it 				   */
	    if (!got_rule)
	    {
		char *tmp;
		tmp = tempcell (filecnt);
		sprintf (tmp_names[filecnt], "%s", G_fully_qualified_name(tmp,G_mapset()));
		fprintf(tmpfp, "r.reclass input='%s' output='%s' title='Weight temp %d' << EOF%d\n",
			old_name, tmp, filecnt, filecnt);
		if (*title)
		    strcat (title, ",");
		else
		    sprintf (title, "weighted overlay of ");
		strcat (title, old_name);
	    }
	}

	switch (parse (buf))
	{
	    case -1:
		if (istty)
		{
		    fprintf (stderr, "illegal weight rule. ignored\n");
		}
		else
		{
		    strcat (buf, " - invalid weight rule");
		    G_fatal_error (buf);
		    exit(1);
		}
		break;

	    case 0: break;

	    default: 
		/* write rule out to file */
		fprintf (tmpfp, "%s\n", buf);
		got_rule = 1;
		break;
	}
    }
    do_end (tmpfp, filecnt, &got_old, &got_rule);

    fprintf (tmpfp, "r.mapcalc << EOF\n");
    fprintf (tmpfp, "%s = ", result);
    for (i = 1 ; i <= MAXFILS && strlen (tmp_names[i]) ; i++)
	fprintf (tmpfp, "%s'%s'", (i == 1 ? "" : acts[action]), tmp_names[i]);
    fprintf (tmpfp, "\nEOF\n");
}

static int
do_end (fp, filecnt, got_old, got_rule)
    FILE *fp;
    int filecnt, *got_old, *got_rule;
{
    if (!*got_old)	/* first time */
    {
	strcpy (old_name, buf);
	*got_old = 1;
	return;
    }

    if (*got_old && *got_rule)
    {
	fprintf (fp, "EOF%d\n", filecnt);
	strcpy (old_name, buf);
	*got_rule = 0;
	*got_old = 0;
    }
    else
	/* have old but no rules, so dont call reclass! */
	if (*got_old && !*got_rule)
	{
	    strcpy (tmp_names[filecnt], old_name); 
	    strcpy (old_name, buf);
	    *got_old = 0;
	}
}

static char *
tempcell (num)
    int num;
{
    char letter;
    static char tfile[256];

    letter = 'A';
    while (letter <= 'Z')
    {
	sprintf (tfile, "WEIGHT.%d%c", num, letter);
	if (G_find_cell (tfile, G_mapset ()) != NULL)
	    letter++;
	else 
	    return (tfile);
    }
    return (NULL);
}

numeric (str)
    char *str;
{
    for ( ; *str ; str++)
	if (*str < '0' || *str > '9')
	    return (0);
    return (1);
}

/* remove all quotes from string */
char *
strip_quotes (str)
    char *str;
{
    char *t, *f;

    t = f = str;
    for (; *f ; f++)
	if (*f != '"')
	    *t++ = *f;
    return (str);
}
