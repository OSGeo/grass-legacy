#include "config.h"
#include <stdio.h>

#define CAPTURE(X) strncpy (X, value, sizeof(X)); X[sizeof(X) - 1] = 0
#define FIELD(X) strcmp(X,field)==0

gather (in)
    FILE *in;
{
    char buf[200];
    char value[100];
    char field[100];

    while (fgets (buf, sizeof buf, in))
   {
	*value = 0;
	*field = 0;
	if (sscanf (buf,"%[^:]:%[^\n]", field, value) < 1)
		continue;
	if (FIELD("text") || FIELD("#text"))
	{
	    int i, t;
	    char *v;

	    strcpy (config.skip,FIELD("text")?"no":"yes");
	    i = 0;
	    t = 0;
	    v = value;
	    while (*v)
	    {
		if (v[0] == '\\' && v[1] == 'n')
		{
		    config.text[t][i] = 0;
		    if (++t >= TEXTLINES) return 1;
		    i = 0;
		    v += 2;
		}
		else
		{
		    if (i < sizeof(config.text[t]) - 1)
			config.text[t][i++] = *v;
		    v++;
		}
	    }
	    config.text[t][i] = 0;
	    while (++t < TEXTLINES)
		config.text[t][0] = 0;
	    return 1;
	}

	G_strip (value);

	if (FIELD("font"))
        {
            CAPTURE (config.font);
            continue;
        }

	if (FIELD("color"))
	{
	    CAPTURE (config.color);
	    continue;
	}

	if (FIELD("hcolor"))
	{
	    CAPTURE (config.hcolor);
	    continue;
	}

	if (FIELD ("xoffset"))
	{
	    CAPTURE (config.xoffset);
	    continue;
	}

	if (FIELD ("yoffset"))
	{
	    CAPTURE (config.yoffset);
	    continue;
	}

	if (FIELD ("ref"))
	{
	    CAPTURE (config.ref);
	    continue;
	}

	if (FIELD ("background"))
	{
	    CAPTURE (config.background);
	    continue;
	}

	if (FIELD ("border"))
	{
	    CAPTURE (config.border);
	    continue;
	}

	if (FIELD ("opaque"))
	{
	    CAPTURE (config.opaque);
	    continue;
	}

	if (FIELD ("size"))
	{
	    CAPTURE (config.size);
	    continue;
	}

	if (FIELD ("width"))
	{
	    CAPTURE (config.width);
	    continue;
	}

	if (FIELD ("hwidth"))
	{
	    CAPTURE (config.hwidth);
	    continue;
	}

	if (FIELD ("north"))
	{
	    CAPTURE (config.north);
	    continue;
	}

	if (FIELD ("east"))
	{
	    CAPTURE (config.east);
	    continue;
	}
    }
    return 0;
}
