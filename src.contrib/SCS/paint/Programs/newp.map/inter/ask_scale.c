#include "gis.h"

static char *explanation[] =
{
"",
"you may specify the map scale in one of the following ways:",
"",
"  1 : n",
"  1 inch equals n <units>    <units> can be miles, meters, or kilometers",
"  n inches                   absolute horizontal width of output",
"  n panels                   output will fill n panels",
"",
0};

ask_scale (fd)
    FILE *fd;
{
    char **c;
    char buf[200];

    printf ("\nSCALE\n");
    while(1)
    {
	printf ("enter map scale: ");
	input (buf);
	if (scan_scale (buf))
	    break;
	for (c = explanation; *c; c++)
	    printf ("%s\n", *c);
    }
    fprintf (fd, "scale %s\n", buf);
    begin_record ("SCALE:");
    add_record (buf);
    end_record();
}
