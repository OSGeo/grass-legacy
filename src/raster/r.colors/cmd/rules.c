#include "gis.h"

struct rule
{
    int set;
    CELL cat;
    int r,g,b;
} ;

read_color_rules (colors, min, max)
    struct Colors *colors;
    CELL min, max;
{
    struct rule *rule = NULL;
    int nrules = 0;
    int low,high,n;
    int set,r,g,b;
    long cat;

    G_init_colors (colors);
    if (isatty(0))
    {
	printf ("Enter rules, end when done, help if you need it.\n");
	printf ("Data range is %ld to %ld\n", (long)min, (long)max);
    }
    while (read_rule (&cat, &r, &g, &b, &set, min, max))
    {
	n = nrules++;
	rule = (struct rule *) G_realloc (rule, nrules*sizeof(struct rule));
	rule[n].cat = (CELL) cat;
	rule[n].r = r;
	rule[n].g = g;
	rule[n].b = b;
	rule[n].set = set;
    }
    if (nrules == 0) return 0;

/* first rule must be set. if not set cat to min */
    if (!rule[0].set)
    {
	rule[0].cat = min;
	rule[0].set = 1;
    }
    if (nrules == 1)
    {
	G_set_color (rule[0].cat, rule[0].r, rule[0].g, rule[0].b, colors);
	free (rule);
	return 1;
    }

/* last rule must set, if not set cat to max */
    if (!rule[nrules-1].set)
    {
	rule[nrules-1].cat = max;
	rule[nrules-1].set = 1;
    }

/* fill in all unset cats */
    for (n=0; n < nrules; n++)
    {
	if (rule[n].set)
	{
	    low = n;
	    continue;
	}
	for (n++; n < nrules; n++)
	    if (rule[n].set)
		break;
	high = n;
	for (n=low+1; n < high; n++)
	    rule[n].cat = rule[low].cat 
	     + ((rule[high].cat - rule[low].cat) * (n-low)) / (high-low);
	low = high;
    }

/* set the colors now */
    for (low=0,high=1; high < nrules; low++,high++)
	G_add_color_rule (
	      rule[low].cat, rule[low].r, rule[low].g, rule[low].b,
	      rule[high].cat, rule[high].r, rule[high].g, rule[high].b,
	      colors);
    
    return 1;
}

read_rule (cat, r, g, b, set, min, max)
    long *cat;
    int *r, *g, *b, *set;
    CELL min, max;
{
    char buf[1024];
    char color[256];
    int n;
    int line;

    for (line=1;;line++)
    {
	if (isatty(0)) printf ("> ");
	if (!gets(buf)) return 0;
	for (n=0;buf[n];n++)
	    if(buf[n] == ',')
		buf[n] = ' ';
	G_strip(buf);
	if (*buf == 0) continue;
	if (*buf == '#') continue;
	if (strcmp(buf, "end") == 0) return 0;

	if (strcmp(buf, "help") == 0)
	{
	    printf ("Enter a rule in one of these formats:\n");
	    printf (" color\n");
	    printf (" cat color\n");
	    printf (" n%% color\n");
	    printf ("color can be one of:\n");
	    show_colors (stdout);
	    printf ("or an r,g,b triplet, e.g.: 0 127 255\n");

	    continue;
	}
	if (sscanf (buf, "%d%% %d %d %d", &n, r, g, b) == 4)
	{
	    *cat = min + ((double)max-(double)min)*n/100.0 + .5;
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%ld %d %d %d", cat, r, g, b) == 4)
	{
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%d %d %d", r, g, b) == 3)
	{
	    *cat = 0;
	    *set = 0;
	    return 1;
	}
	if (sscanf (buf, "%d%% %s", &n, color) == 2)
	{
	    if (!lookup_color (color, r,g,b))
	    {
		badrule(buf,line);
		continue;
	    }
	    *cat = min + ((double)max-(double)min)*n/100.0 + .5;
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%ld %s", cat, color) == 2)
	{
	    if (!lookup_color (color, r,g,b))
	    {
		badrule(buf,line);
		continue;
	    }
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%s", color) == 1)
	{
	    if (!lookup_color (color, r,g,b))
	    {
		badrule(buf,line);
		continue;
	    }
	    *cat = 0;
	    *set = 0;
	    return 1;
	}
	badrule(buf,line);
    }
}

badrule(s,line)
    char *s;
{
    if (!isatty(0))
	fprintf (stderr, "%s:line %d:%s\n", G_program_name(), line,  s);
    fprintf (stderr, "** bad color specification **\n");
    if (!isatty(0))
	exit(1);
}

lookup_color (color, r, g, b)
    char *color;
    int *r, *g, *b;
{
    float fr, fg, fb;

    if (G_color_values(color, &fr, &fg, &fb) > 0)
    {
	*r = (int)(fr * 255);
	*g = (int)(fg * 255);
	*b = (int)(fb * 255);
	return 1;
    }
    if (isatty(0))
    {
	fprintf (stderr, "%s - unknown color\n", color);
	fprintf (stderr, "Valid colors are:\n");
	show_colors (stderr);
    }
    return 0;
}

show_colors (fd)
    FILE *fd;
{
    int len;
    int i,n;
    char *color;
    char *G_color_name();

    len = 0;
    for (i = 0; color = G_color_name(i); i++)
    {
	n = strlen (color) + 1;
	if (len + n > 78)
	{
	    fprintf (fd, "\n");
	    len = 0;
	}
	fprintf (fd, " %s", color);
	len += n;
    }
    fprintf (fd, "\n");
}
