#include "gis.h"
main(argc,argv) char *argv[];
{
    struct Option *input, *output, *rows;
    int n;

    G_gisinit (argv[0]);

    input = G_define_option();
    input->key             = "input";
    input->type            = TYPE_STRING;
    input->required        = YES;
    input->multiple        = NO;
    input->gisprompt       = "old,dig,vector";
    input->description     = "vector input file";

    output = G_define_option();
    output->key            = "output";
    output->type           = TYPE_STRING;
    output->required       = YES;
    output->multiple       = NO;
    output->gisprompt      = "new,cell,raster";
    output->description    = "raster output file";

    rows = G_define_option();
    rows->key              = "rows";
    rows->type             = TYPE_INTEGER;
    rows->required         = NO;
    rows->multiple         = NO;
    rows->description      = "number of rows to hold in memory";
    rows->answer           = "512";

    if (G_parser (argc, argv))
	exit (-1);

    sscanf (rows->answer, "%d", &n);
    exit(vect_to_rast (input->answer, output->answer, n));
}
