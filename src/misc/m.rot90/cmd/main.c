#include <unistd.h>
#include <fcntl.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    char *infile;
    char *outfile;
    int in,out;
    int verbose;
    int nrows ;
    int ncols ;
    int bpc ;

	struct GModule *module;
    struct
    {
	struct Option *input, *output, *rows, *cols, *bpc;
    } parm;
    struct
    {
	struct Flag *q;
    } flag;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Rotates elevation data extracted by either m.dted.extract "
		"or m.dmaUSGSread.";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "File to be rotated";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->description = "Output file";

    parm.rows = G_define_option();
    parm.rows->key = "rows";
    parm.rows->type = TYPE_INTEGER;
    parm.rows->required = YES;
    parm.rows->description = "Number of rows in the input file";

    parm.cols = G_define_option();
    parm.cols->key = "cols";
    parm.cols->type = TYPE_INTEGER;
    parm.cols->required = YES;
    parm.cols->description = "Number of columns in the input file";

    parm.bpc = G_define_option();
    parm.bpc->key = "bpc";
    parm.bpc->type = TYPE_INTEGER;
    parm.bpc->required = YES;
    parm.bpc->description = "Number of bytes per cell (i.e. per data value)";

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "Run quietly";

    if (G_parser(argc,argv))
	exit(1);


    verbose = !flag.q->answer;
    infile = parm.input->answer;
    outfile = parm.output->answer;
    nrows = ncols = bpc = 0;
    sscanf (parm.rows->answer, "%d", &nrows);
    sscanf (parm.cols->answer, "%d", &ncols);
    sscanf (parm.bpc->answer, "%d", &bpc);
    if (nrows <= 0 || ncols <= 0 || bpc <= 0)
    {
	G_usage();
	exit(1);
    }

/* output file must not exist */
    if (access (outfile,0) == 0)
    {
	fprintf (stderr, "%s: %s - output file exists. Sorry!\n",
		G_program_name(), outfile);
	exit(1);
    }

/* open the files */
    in = open (infile,0);
    if (in < 0)
    {
	fprintf (stderr, "%s: ", G_program_name());
	perror (infile);
	exit(1);
    }
    out = creat (outfile, 0666);
    if (out < 0)
    {
	fprintf (stderr, "%s: ", G_program_name());
	perror (outfile);
	exit(1);
    }

/* do the rotation */
    rotate (in, out, nrows, ncols, bpc, verbose);

/* close the files and leave */
    close(in);
    close(out);

    exit(0);
}
