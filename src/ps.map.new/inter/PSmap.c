#include <stdio.h>
#include <unistd.h>
#include "gis.h"

int PSmap (char *scriptfile, char *ps_outfile, int background)
{
    char input[1024],
	 output[1024];

    sprintf(input, "input=%s", scriptfile);
    sprintf(output, "output=%s", ps_outfile);
    if (background && (G_fork() > 0)) exit(0);
    execlp("ps.map.new", "ps.map.new", input, output, 0);
    G_fatal_error("unable to exec ps.map.new");
}
