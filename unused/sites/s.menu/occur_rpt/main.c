#include <unistd.h>
#include "gis.h"
#include "report.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    REPORT *layers;
    REPORT *ref;
    FILE *out;

    if (argc != 3)
	    exit(0);
    G_gisinit (argv[0]);

    out = fopen (argv[2], "w");
    if (!out)
    {
	perror (argv[2]);
	exit(-1);
    }
    layers = report_open (argv[1]);
    ref    = report_open (argv[1]);

    title (out,ref);
    divider (out);

    announce ("\n");

    report_seek_layers (layers);
    while (report_read_record (layers,"layer"))
    {
	announce ("\n");
	announce (layers->field[2]);
	process (out,layers, ref);
    }
    announce ("\n");

    fclose (out);
    exit(0);
}
