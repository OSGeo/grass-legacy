#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct Option *input, *output, *rows;
    int n;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Converts a binary GRASS vector map layer "
		"into a GRASS raster map layer.";

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
