#include "gis.h"
main(argc,argv) char *argv[];
{
    struct Option *input, *output, *title, *rows;
    int n;

    G_gisinit (argv[0]);

    input = G_define_option();
    input->key             = "input";
    input->type            = TYPE_STRING;
    input->required        = YES;
    input->multiple        = NO;
    input->description     = "Input file";

    output = G_define_option();
    output->key            = "output";
    output->type           = TYPE_STRING;
    output->required       = YES;
    output->multiple       = NO;
    output->gisprompt      = "new,cell,raster";
    output->description    = "Raster output file";

    title                  = G_define_option();
    title->key             = "title";
    title->key_desc        = "\"phrase\"";
    title->type            = TYPE_STRING;
    title->required        = NO;
    title->description     = "Title for resultant raster map";

    rows = G_define_option();
    rows->key              = "rows";
    rows->type             = TYPE_INTEGER;
    rows->required         = NO;
    rows->multiple         = NO;
    rows->description      = "Number of rows to hold in memory";
    rows->answer           = "512";

    if (G_parser (argc, argv))
	exit (-1);

    sscanf (rows->answer, "%d", &n);
    exit(poly_to_rast (input->answer, output->answer, title->answer, n));
}
