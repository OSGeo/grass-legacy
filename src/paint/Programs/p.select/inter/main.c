#include "gis.h"
#include "proto.h"

int main(int argc, char *argv[])
{
    char buf[1024];
    char name[1024];
    int have_painter;

    G_gisinit (argv[0]);

    do
    {
	list_painters();
	fprintf (stdout,"\n");
	have_painter = show_current_painter();

	if (have_painter)
	    fprintf (stdout,"Hit RETURN to continue, or select new painter> ");
	else
	    fprintf (stdout,"Select a painter, or hit RETURN to quit> ");
	if (!G_gets(buf))
	    continue;
	if (sscanf (buf, "%s", name) != 1)
	    exit(0);
    }
    while (!select_painter(name));

  return 0;
}
