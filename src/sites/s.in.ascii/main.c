#include "gis.h"


main (argc, argv) char *argv[];
{
    char *output, *input;
    double east, north;
    char desc[1024];
    char *fs;
    int stat;
    FILE *in_fd, *out_fd;
    struct
    {
	struct Option *input, *output, *fs;
    } parm;

    G_gisinit (argv[0]);

    parm.output = G_define_option();
    parm.output->key = "sites";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->description = "sites file to be created";
    parm.output->gisprompt = "any,site_lists,sites";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = NO;
    parm.input->description = "unix file containing sites";

    parm.fs = G_define_option();
    parm.fs->key = "fs";
    parm.fs->key_desc = "character|space|tab";
    parm.fs->type = TYPE_STRING;
    parm.fs->required = NO;
    parm.fs->description = "input field separator";
    parm.fs->answer = "space";

    if (G_parser(argc,argv))
	exit(1);
    if(input = parm.input->answer)
    {
	in_fd = fopen (input, "r");
	if (NULL == in_fd)
	{
	    fprintf (stderr, "%s - ", G_program_name());
	    perror (input);
	    exit(1);
	}
    }
    else
	in_fd = stdin;

    output = parm.output->answer;

    fs = parm.fs->answer;
    if (fs != NULL)
    {
	if(strcmp (fs, "space") == 0)
	    fs = NULL;
	else if(strcmp (fs, "tab") == 0)
	    fs = NULL;
    }

    out_fd = G_fopen_sites_new (output);
    if (out_fd == NULL)
    {
	fprintf (stderr, " %s - can't create sites file [%s]",
		G_program_name(), output);
	exit(1);
    }

    while (0 != (stat = get_site (in_fd, &east, &north, desc, fs)))
	if (stat > 0)
	    G_put_site (out_fd, east, north, desc);
    fclose (out_fd);
    exit(0);
}
