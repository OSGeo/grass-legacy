#include "config.h"
#include <stdio.h>

update (out)
    FILE *out;
{
    int i;
    int lines;

    lines = 0;
    for (i = 0; i < TEXTLINES; i++)
	if (config.text[i][0])
	    lines++;

    if (lines == 0)
	return 0;

    output (out, "east", config.east);
    output (out, "north", config.north);

    output (out, "xoffset", config.xoffset);
    output (out, "yoffset", config.yoffset);
    output (out, "ref", config.ref);
    output (out, "font", config.font);
    output (out, "color", config.color);
    output (out, "size", config.size);
    output (out, "width", config.width);
    output (out, "hcolor", config.hcolor);
    output (out, "hwidth", config.hwidth);
    output (out, "background", config.background);
    output (out, "border", config.border);
    output (out, "opaque", config.opaque);

    fprintf (out, "\n");
    if (!(strcmp (config.skip,"yes") && strcmp (config.skip,"y")
    &&    strcmp (config.skip,"YES") && strcmp (config.skip,"Y")))
	fprintf (out, "#");
    fprintf (out, "text:");

    lines = 0;
    for (i = 0; i < TEXTLINES; i++)
	if (config.text[i][0])
	{
	    if (lines++ > 0)
		fprintf (out,"\\n");
	    fprintf (out,"%s", config.text[i]);
	}

    fprintf(out,"\n\n");
    return 1;
}
