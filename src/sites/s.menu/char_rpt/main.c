#include <unistd.h>
#include "gis.h"
#include "report.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    REPORT *points;
    REPORT *ref;
    int *cats_list;
    int n;
    FILE *out;

    if (argc != 3)
	exit(0);
    G_gisinit (argv[0]);

    out = fopen (argv[2],"w");
    if (!out)
    {
	perror (argv[2]);
	exit(-1);
    }
    points = report_open (argv[1]);
    ref    = report_open (argv[1]);

    cats_list = (int *) G_malloc (ref->matrix.size * sizeof(int));

    title (out, ref);
    divider (out);

    report_seek_points (points);
    counter_reset ("\n",0);
    counter (points->npoints);
    counter_reset (" points ... ",0);
    for (n = 1; report_read_record (points,"point"); n++)
    {
	counter (n);
	process (out, points, ref, cats_list);
    }
    counter_reset ("\n",0);

    fclose (out);
    exit(0);
}
