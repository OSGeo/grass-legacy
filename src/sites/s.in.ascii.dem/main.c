/* Routine for the import of ASCII-sitelists. Original routine s.in.ascii
   was changed for modules like s.to.rast. That module needs the categories in
   the #cat-format (with #-character in first position), s.in.ascii
   writes out the simple cat-format without # */

/* Modification by Markus Neteler   11.12.96
   email: neteler@geog.uni-hannover.de
   Hannover, Germany */


#include "gis.h"
main (argc, argv) char *argv[];
{
    char *output, *input;
    double east, north;
    char desc[1024];
    char temp[1025];
    int i;
    int length;
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

/* a new temp-variable. The #-character is needed for s.to.rast etc.*/
    temp[0] = '#';
    while (0 != (stat = get_site (in_fd, &east, &north, desc, fs)))
	if (stat > 0)
        {  
            /* copy desc-String with #-character in the first position 
               into temp-string*/
            length = strlen(desc);
            for( i = 0; i<length; i++ )
                temp[i+1] = desc[i];
	    G_put_site (out_fd, east, north, temp);
        }
    fclose (out_fd);
    exit(0);
}
