#include "gis.h"
#define MAIN
#include "config.h"
char *element = "paint/labels" ;
main(argc, argv) char *argv[];
{
    char name[30];
    char *mapset;
    char *tempfile;
    FILE *in, *out;
    int stat;

    G_clear_screen();
    printf ("PAINT LABELS\n");
    G_gisinit (argv[0]);

    tempfile = G_tempfile();

    mapset = G_ask_any ("enter new or existing labels file", name, element, "labels", 0);
    if (!mapset)
	exit(0);

    out = fopen (tempfile, "w");
    if (!out)
    {
	perror (tempfile);
	exit(0);
    }

    in = G_fopen_old (element, name, mapset);

    G_zero (&config, sizeof config);
    strcpy (config.ref,"center");
    strcpy (config.color,"black");
    strcpy (config.width,"1");
    strcpy (config.hcolor,"none");
    strcpy (config.hwidth,"0");
    strcpy (config.background,"white");
    strcpy (config.border,"black");
    strcpy (config.size,"500");
    strcpy (config.opaque,"yes");
    strcpy (config.font,"standard");

    stat = process_old (in, out, name);
    if (stat)
	stat = process_new (out, name);

    fclose (out);
    if (in)
	fclose (in);
    if (stat && config.count)
    {
	int in, out;
	int n;
	char buf[1024];

	in = open (tempfile, 0);
	if (in < 0)
	{
	    perror (tempfile);
	    exit(1);
	}
	out = G_open_new (element, name);
	if (out < 0)
	{
	    perror (name);
	    exit(1);
	}

	while ((n = read (in, buf, sizeof buf)) > 0)
	    write (out, buf, n);

	close (in);
	close (out);
    }
    else if (stat)
	G_remove (element, name);
    unlink (tempfile);
}
