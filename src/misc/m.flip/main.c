#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	struct GModule *module;

    char *infile;
    char *outfile;
    int in,out;

    int verbose;
    int nrows;
    int ncols;
    int bpc;

    struct
    {
	struct Option *input, *output, *rows, *cols, *bpc;
    } parm;
    struct
    {
	struct Flag *q;
    } flag;

	module = G_define_module();
	module->description =
		"Flips elevation data extracted from systems that retrieve "
		"data by rows from south to north.";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "File to be flipped";

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
    parm.rows->options = "1-9999999";

    parm.cols = G_define_option();
    parm.cols->key = "cols";
    parm.cols->type = TYPE_INTEGER;
    parm.cols->required = YES;
    parm.cols->description = "Number of columns in the input file";
    parm.cols->options = "1-9999999";

    parm.bpc = G_define_option();
    parm.bpc->key = "bpc";
    parm.bpc->type = TYPE_INTEGER;
    parm.bpc->required = YES;
    parm.bpc->description = "Number of bytes per cell (ie, per data value)";
    {
      static char str[20];
      sprintf (str, "1-%ld", sizeof (CELL));
      parm.bpc->options = str;
    }

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "Run quietly";

    if (G_parser(argc, argv))
	exit(1);
    
    verbose = !flag.q->answer;
    infile = parm.input->answer;
    outfile = parm.output->answer;
    scan_int(parm.rows, &nrows);
    scan_int(parm.cols, &ncols);
    scan_int(parm.bpc, &bpc);

/* output file must not exist */
    if (access (outfile,0) == 0)
    {
	fprintf (stderr, "%s: %s - file exists. Sorry!\n", argv[0], outfile);
	exit(1);
    }

/* open the files */
    in = open (infile,0);
    if (in < 0)
    {
	perror (infile);
	exit(1);
    }
    out = creat (outfile, 0666);
    if (out < 0)
    {
	perror (outfile);
	exit(1);
    }

/* do the rotation */
    flip (in, out, nrows, ncols, bpc, verbose);

/* close the files and leave */
    close(in);
    close(out);

    exit(0);
}

int scan_int (struct Option *parm, int *n)
{
    if (sscanf (parm->answer, "%d", n) != 1 || *n <= 0)
    {
	fprintf (stderr, "%s=%s - must be a positive number\n",
		parm->key, parm->answer);
	G_usage();
	exit(1);
    }

    return *n;
}
