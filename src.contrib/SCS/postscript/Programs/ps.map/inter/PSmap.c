#include <stdio.h>

PSmap(scriptfile, ps_outfile, background)
char *scriptfile, ps_outfile;
int background;
{
    char arg[1024];

    sprintf(arg, "input=%s output=%s", scriptfile, ps_outfile);
    if (background && (G_fork() > 0)) exit(0);
    execlp("ps.map", "ps.map", arg, 0);
    G_fatal_error("unable to exec ps.map");
}
