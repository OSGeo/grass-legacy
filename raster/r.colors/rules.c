#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "gis.h"
#include "local_proto.h"

struct rule
{
    int set;
    DCELL val;
    int r,g,b;
} ;

/* color structure for default and null value */
struct colr {
    int r,g,b;
    int set;
};

int read_color_rules (struct Colors *colors, DCELL min, DCELL max, int fp)
{
    struct rule *rule = NULL;
    struct colr df, null;
    int nrules = 0;
    int low,high,n;
    int set,nv,others,r,g,b;
    double val;

    /* initialization */
    df.r = df.g = df.b = df.set = 0;
    null.r = null.g = null.b = null.set = 0;

    G_init_colors (colors);
    if (isatty(0))
    {
	fprintf (stdout, "Enter rules, \"end\" when done, \"help\" if you need it.\n");
	if (fp)
	  fprintf (stdout, "fp: Data range is %.25f to %.25f\n", (double)min, (double)max);
	else
	  fprintf (stdout, "Data range is %ld to %ld\n", (long)min, (long)max);
    }
    while (read_rule (&val, &r, &g, &b, &set, &nv, &others, min, max))
    {
	n = nrules++;
	rule = (struct rule *) G_realloc (rule, nrules*sizeof(struct rule));
	if (!set) {
	  if (others) {
	    df.r = r;
	    df.g = g;
	    df.b = b;
	    df.set = 1;
	    nrules--;
	  }
	  else if (nv) {
	    null.r = r;
            null.g = g;
            null.b = b;
            null.set = 1;
            nrules--;
          }
	}
	else {
	  rule[n].val = (DCELL) val;
	  rule[n].r = r;
	  rule[n].g = g;
	  rule[n].b = b;
	  rule[n].set = set;
	}
    }
    if (nrules == 0) return 0;

/* first rule must be set. if not set val to min */
    if (!rule[0].set)
    {
	rule[0].val = min;
	rule[0].set = 1;
    }
    if (nrules == 1)
    {
      if (fp)
	G_set_d_color(rule[0].val, rule[0].r, rule[0].g, rule[0].b, colors);
      else
	G_set_color((CELL)rule[0].val, rule[0].r, rule[0].g, rule[0].b, colors);
	G_free (rule);
      if (null.set)
	G_set_null_value_color(null.r, null.g, null.b, colors);
      if (df.set)
	G_set_default_color(df.r, df.g, df.b, colors);
	return 1;
    }

/* last rule must set, if not set val to max */
    if (!rule[nrules-1].set)
    {
	rule[nrules-1].val = max;
	rule[nrules-1].set = 1;
    }

    if(rule[0].val > min || rule[nrules-1].val < max)
       G_warning("Your color rules do not cover the whole range of data!");

/* fill in all unset val */
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
	    rule[n].val = rule[low].val 
	     + ((rule[high].val - rule[low].val) * (n-low)) / (high-low);
	low = high;
    }

/* set the colors now */
    for (low=0,high=1; high < nrules; low++,high++) {
	if (fp)
	  G_add_d_raster_color_rule (
	      &rule[low].val, rule[low].r, rule[low].g, rule[low].b,
	      &rule[high].val, rule[high].r, rule[high].g, rule[high].b,
	      colors);
	else
	  G_add_color_rule (
	      (CELL) rule[low].val, rule[low].r, rule[low].g, rule[low].b,
	      (CELL) rule[high].val, rule[high].r, rule[high].g, rule[high].b,
	      colors);
    }
    /* null value and default color set up, if rules are set up by user */
    if (null.set) {
	G_set_null_value_color(null.r, null.g, null.b, colors);
    }
    if (df.set) {
    	G_set_default_color(df.r, df.g, df.b, colors);
    }
    return 1;
}

int read_rule (double *val, int *r, int *g, int *b, int *set, int *nvalue, int *others,
               DCELL min, DCELL max)
{
    char buf[1024];
    char color[256];
    int i;
    double n;
    int line;
    char tmpstr[16];

    *set = *nvalue = *others = 0;
    for (line=1;;line++)
    {
	if (isatty(0)) fprintf (stdout,"> ");
	if (!fgets(buf,1024,stdin)) return 0;
	for (i=0;buf[i];i++)
	    if(buf[i] == ',')
		buf[i] = ' ';
	G_strip(buf);
	if (*buf == 0) continue;
	if (*buf == '#') continue;
	if (strncmp(buf, "end", 3) == 0) return 0;

	if (strncmp(buf, "help", 4) == 0)
	{
	    fprintf (stdout, "Enter a rule in one of these formats:\n");
	    fprintf (stdout, " color\n");
	    fprintf (stdout, " val color\n");
	    fprintf (stdout, " n%% color\n");
	    fprintf (stdout, " nv color\n");
	    fprintf (stdout, " default color\n");
	    fprintf (stdout, "color can be one of:\n");
	    show_colors (stdout);
	    fprintf (stdout, "or an r,g,b triplet, e.g.: 0 127 255\n");

	    continue;
	}
	if (sscanf (buf, "%lf%% %d %d %d", &n, r, g, b) == 4)
	{
	    if (n<0 || n>100 || *r<0 || *r>255 || *g<0 || *g>255 ||
		*b<0 || *b>255)
	    {
		badrule(buf,line);
		continue;
	    }
	    *val = min + ((double)max-(double)min)*(n+0.5)/100.0;
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%lf %d %d %d", val, r, g, b) == 4)
	{
            if (/*(*val < (double) min) || (*val > (double) max) || 
		commented out by Olga */ *r<0 || *r>255
		|| *g<0 || *g>255 || *b<0 || *b>255)
            {
                fprintf(stderr, "** warning: no such value **\n");
                fprintf(stderr, "** rule is not added **\n");
                continue;
            }
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%s %d %d %d", tmpstr, r, g, b) == 4)
	{
	    if (*r<0 || *r>255 || *g<0 || *g>255 || *b<0 || *b>255)
	    {
		fprintf(stderr, "** warning: no such value **\n");
                fprintf(stderr, "** rule is not added **\n");
                continue;
            }
	    if (!strcmp("nv", tmpstr)) {
	        *nvalue = 1;
            	return 1;
	    }
	    else if (!strcmp("default", tmpstr)) {
		*others = 1;
		return 1;
	    }
	    else {
	    	fprintf(stderr, "** rule is not added **\n");
	    	continue;
	    }
        }
	if (sscanf (buf, "%d %d %d", r, g, b) == 3)
	{
	    if (*r<0 || *r>255 || *g<0 || *g>255 || *b<0 || *b>255) {
		badrule(buf, line);
		continue;
	    }
	    *val = 0;
	    *set = 0;
	    return 1;
	}
	if (sscanf (buf, "%lf%% %s", &n, color) == 2)
	{
	    if (!lookup_color (color, r,g,b) || n < 0 || n > 100)
	    {
		badrule(buf,line);
		continue;
	    }
	    *val = min + ((double)max-(double)min)*(n+0.5)/100.0;
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%lf %s", val, color) == 2)
	{
	    if (!lookup_color (color, r,g,b))
	    {
		badrule(buf,line);
		continue;
	    }
	    /* commented by olga because canb't cover whole fp range 
	    if ((*val < (double) min) || (*val > (double) max))
	    {
		fprintf(stderr, "** warning: no such value **\n");
		fprintf(stderr, "** rule is not added **\n");
		continue;
	    }
	    */
	    *set = 1;
	    return 1;
	}
        if (sscanf (buf, "%s %s", tmpstr, color) == 2)
        {
            if (!lookup_color (color, r,g,b))
            {
                badrule(buf,line);
                continue;
            }
	    if (!strcmp("nv", tmpstr)) {
	    	*nvalue = 1;
            	return 1;
	    }
	    else if (!strcmp("default", tmpstr)) {
		*others= 1;
		return 1;
	    }
	    else {
	    	fprintf(stderr, "** rule is not added **\n");
	    	continue;
	    }
        }
	if (sscanf (buf, "%s", color) == 1)
	{
	    if (!lookup_color (color, r,g,b))
	    {
		badrule(buf,line);
		continue;
	    }
	    *val = 0;
	    *set = 0;
	    return 1;
	}
	badrule(buf,line);
    }
    return 0;
}

int badrule (char *s, int line)
{
    if (!isatty(0))
	fprintf (stderr, "%s:line %d:%s\n", G_program_name(), line,  s);
    fprintf (stderr, "** bad color specification **\n");
    fprintf (stderr, "** rule is not added **\n");
    if (!isatty(0))
	exit(1);

    return 0;
}

int lookup_color (char *color, int *r, int *g, int *b)
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

int show_colors (FILE *fd)
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
    
    return 0;
}
