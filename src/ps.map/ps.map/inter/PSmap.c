#include <stdio.h>

PSmap(scriptfile, ps_outfile, background)
char *scriptfile, *ps_outfile;
int background;
{
    char input[1024],
	 output[1024];

    sprintf(input, "input=%s", scriptfile);
    sprintf(output, "output=%s", ps_outfile);
    if (background && (G_fork() > 0)) exit(0);
    execlp("ps.map", "ps.map", input, output, 0);
    G_fatal_error("unable to exec ps.map");
}
