/* %W% %G% */

#include "gis.h"

#define INTERACTIVE "Interactive Commands","[0-9a-z]*"
#define NON_INTERACTIVE "Non-Interactive Commands","[A-Z]*"

static char *tempfile;
FILE *opentemp();

main (argc, argv) char *argv[];
{
    char entry[1024];
    int ok;

    G_gisinit (argv[0]);
    tempfile = G_tempfile();


    while (1)
    {
	G_clear_screen();
	printf ("GRASS online help\n");
	do
	{
	    printf ("\n");
	    printf ("Enter command for which you would like help?\n");
	    printf ("Enter \"L\" for a list of GRASS commands\n");
	    printf ("Enter \"A\" to get help for all GRASS commands\n");
	    printf ("Hit RETURN to quit\n");
	    printf ("> ");
	}
	while (!G_gets(entry));
	G_strip (entry);
	if (*entry == 0)
	    exit(0);
	if (strcmp (entry, "l") == 0 || strcmp (entry, "L") == 0)
	{
	    printf ("one moment ...");
	    fflush (stdout);
	    list ();
	}
	else if (strcmp (entry, "a") == 0 || strcmp (entry, "A") == 0)
	{
	    printf ("one moment ...");
	    fflush (stdout);
	    show_all ();
	}
	else
	{
	    fprintf (stdout,
"------------------------------------------------------------------------\n");
	    ok = show (stdout, entry);
	    fprintf (stdout,
"------------------------------------------------------------------------\n");
	    if (manual(entry, 1))
	    {
		if(G_yes ("Would you like to see the manual entry? ", 0))
		{
		    G_clear_screen();
		    manual (entry, 0);
		}
	    }
	    else
	    {
		if (ok >= 0)
		    printf ("Note: manual entry not available\n\n");
		printf ("Hit RETURN -->");
		G_gets(entry);
	    }
	}
    }
}

show_all ()
{
    FILE *out;

    out = opentemp ("w");
    prepare_show (out, INTERACTIVE);
    prepare_show (out, NON_INTERACTIVE);
    fclose (out);

    more_print ();
}

prepare_show (out, title, pattern)
    FILE *out;
    char *title, *pattern;
{
    char buf[1024];
    FILE *ls, *popen();

    fprintf (out, "%s\n", title);
    fprintf (out,
"------------------------------------------------------------------------\n");
    sprintf (buf, "cd %s/bin; ls %s", G_gisbase(), pattern);
    ls = popen (buf, "r");
    while (G_getl (buf, sizeof buf, ls))
    {
	show (out, buf);
	fprintf (out, "\n");
    }
    pclose (ls);
}

show (out, entry)
    char *entry;
    FILE *out;
{
    char buf[1024];
    FILE *in;

    sprintf (buf, "%s/bin/%s", G_gisbase(), entry);
    fprintf (out, "%s\n", entry);
    if (access (buf, 0) != 0)
    {
	fprintf(out, "    ** no such GRASS command **\n");
	return -1;
    }

    sprintf (buf, "%s/man/help/%s", G_gisbase(), entry);
    if ((in = fopen (buf, "r")) == NULL)
    {
	fprintf(out, "    ** no help available **\n");
	return 0;
    }

/* show entry */
    while (fgets (buf, sizeof buf, in))
	fprintf (out, "    %s", buf);

    fclose (in);
    return 1;
}

list ()
{
    unlink (tempfile);

    prepare_list (INTERACTIVE);
    prepare_list (NON_INTERACTIVE);

    more_print ();
}

more_print()
{
    char buf[1024];

    G_clear_screen();
    sprintf (buf, "more -d %s", tempfile);
    system (buf);
    if (G_yes("\nWould you like to send this list to the printer? ", 0))
    {
	printf ("Sending to the printer ...\n");
	sprintf (buf, "lpr %s", tempfile);
	system (buf);
    }
}

prepare_list (title, pattern)
    char *title, *pattern;
{
    char buf[1024];
    FILE *fd;

    fd = opentemp ("a");
    fprintf (fd, "%s\n", title);
    fprintf (fd,
"------------------------------------------------------------------------\n");
    fclose (fd);
    sprintf (buf, "(cd %s/bin; ls -C %s) >> %s", G_gisbase(), pattern, tempfile);
    system (buf);
    fd = opentemp ("a");
    fprintf (fd, "\n");
    fclose (fd);
}

FILE *
opentemp (mode)
    char *mode;
{
    FILE *fd;
    if(fd = fopen (tempfile, mode))
	return fd;
    G_fatal_error ("Can't open any temp files");
}
