#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "gis.h"
#include "local_proto.h"
#include "glocale.h"

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

static int read_rule(FILE *, double *, int *, int *, int *, int *, int *, int *, DCELL, DCELL);
static int badrule(int, char *, int);
static int lookup_color(int, char *, int *, int *, int *);
static int show_colors(FILE *);

int read_color_rules(
	FILE *fp,
	struct Colors *colors, int quiet, DCELL min, DCELL max, int is_fp)
{
    struct rule *rule = NULL;
    struct colr df, null;
    int nrules = 0;
    int low,high,n;
    int set,nv,others,r,g,b;
    double val, rulemin, rulemax;

    /* initialization */
    df.r = df.g = df.b = df.set = 0;
    null.r = null.g = null.b = null.set = 0;

    G_init_colors (colors);
    if (isatty(fileno(fp)))
    {
	fprintf (stdout, _("Enter rules, \"end\" when done, \"help\" if you need it.\n"));
	if (is_fp)
	  fprintf (stdout, _("fp: Data range is %.25f to %.25f\n"), (double)min, (double)max);
	else
	  fprintf (stdout, _("Data range is %ld to %ld\n"), (long)min, (long)max);
    }
    while (read_rule(fp, &val, &r, &g, &b, &set, &nv, &others, min, max))
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
      if (is_fp)
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

/* figure out max and min of new rules */
    rulemin=rule[0].val; /* rule 0 is always set */
    rulemax=rule[0].val;
    for (n=0; n<nrules; n++) {
	if(! rule[n].set) continue;
	if(rulemin > rule[n].val) rulemin = rule[n].val;
	if(rulemax < rule[n].val) rulemax = rule[n].val;
    }
    G_debug(3, "rulemin=%.1f rulemax=%.1f", rulemin, rulemax);
    if((rulemin > min || rulemax < max) && !quiet)
	G_warning(_("Your color rules do not cover the whole range of data!"));

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
	if (is_fp)
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

static int read_rule(
	FILE *fp,
	double *val, int *r, int *g, int *b, int *set, int *nvalue, int *others,
	DCELL min, DCELL max)
{
    char buf[1024];
    char color[256];
    int i;
    double n;
    int line;
    char tmpstr[16];
    int tty = isatty(fileno(fp));

    *set = *nvalue = *others = 0;
    for (line=1;;line++)
    {
	if (tty)
	    fprintf (stdout,"> ");
	if (!fgets(buf,1024,fp)) return 0;
	for (i=0;buf[i];i++)
	    if(buf[i] == ',')
		buf[i] = ' ';
	G_strip(buf);
	if (*buf == 0) continue;
	if (*buf == '#') continue;
	if (strncmp(buf, "end", 3) == 0) return 0;

	if (strncmp(buf, "help", 4) == 0)
	{
	    fprintf (stdout, _("Enter a rule in one of these formats:\n"));
/*	    fprintf (stdout, _(" color\n")); */ /* does this actually do anything?? */
	    fprintf (stdout, _(" val color\n"));
	    fprintf (stdout, _(" n%% color\n"));
	    fprintf (stdout, _(" nv color\n"));
	    fprintf (stdout, _(" default color\n"));
	    fprintf (stdout, _("color can be one of:\n"));
	    show_colors (stdout);
	    fprintf (stdout, _("or an R:G:B triplet, e.g.: 0:127:255\n"));

	    continue;
	}

	if (sscanf (buf, "%lf%% %d:%d:%d", &n, r, g, b) == 4)
	{
	    if (n<0 || n>100 || *r<0 || *r>255 || *g<0 || *g>255 ||
		*b<0 || *b>255)
	    {
		badrule(tty,buf,line);
		continue;
	    }
	    *val = min + ((double)max-(double)min)*n/100.0;
	    *set = 1;
	    return 1;
	}
#define BACKWARDS_COMPAT
#ifdef BACKWARDS_COMPAT
	if (sscanf (buf, "%lf%% %d %d %d", &n, r, g, b) == 4)
	{
	    if (n<0 || n>100 || *r<0 || *r>255 || *g<0 || *g>255 ||
		*b<0 || *b>255)
	    {
		badrule(tty,buf,line);
		continue;
	    }
	    *val = min + ((double)max-(double)min)*n/100.0;
	    *set = 1;
	    return 1;
	}
#endif

	if (sscanf (buf, "%lf %d:%d:%d", val, r, g, b) == 4)
	{
            if (
               /*(*val < (double) min) || (*val > (double) max) || 
		commented out by Olga */
		*r<0 || *r>255
		|| *g<0 || *g>255 || *b<0 || *b>255)
            {
                fprintf(stderr, _("** warning: R:G:B value(s) out of range [0..255]: %d:%d:%d **\n"), *r, *g, *b);
                fprintf(stderr, _("** rule is not added **\n"));
                continue;
            }
	    *set = 1;
	    return 1;
	}
#ifdef BACKWARDS_COMPAT
	if (sscanf (buf, "%lf %d %d %d", val, r, g, b) == 4)
	{
            if (
               /*(*val < (double) min) || (*val > (double) max) || 
		commented out by Olga */
		*r<0 || *r>255
		|| *g<0 || *g>255 || *b<0 || *b>255)
            {
                fprintf(stderr, _("** warning: R G B value(s) out of range [0..255]: %d %d %d **\n"), *r, *g, *b);
                fprintf(stderr, _("** rule is not added **\n"));
                continue;
            }
	    *set = 1;
	    return 1;
	}
#endif

	if (sscanf (buf, "%s %d:%d:%d", tmpstr, r, g, b) == 4)
	{
	    if (*r<0 || *r>255 || *g<0 || *g>255 || *b<0 || *b>255)
	    {
		fprintf(stderr, _("** warning: no such value **\n"));
                fprintf(stderr, _("** rule is not added **\n"));
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
	    	fprintf(stderr, _("** rule is not added **\n"));
	    	continue;
	    }
        }
#ifdef BACKWARDS_COMPAT
	if (sscanf (buf, "%s %d %d %d", tmpstr, r, g, b) == 4)
	{
	    if (*r<0 || *r>255 || *g<0 || *g>255 || *b<0 || *b>255)
	    {
		fprintf(stderr, _("** warning: no such value **\n"));
                fprintf(stderr, _("** rule is not added **\n"));
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
	    	fprintf(stderr, _("** rule is not added **\n"));
	    	continue;
	    }
        }
#endif

	/* does this actually do anything?? */
/*	if (sscanf (buf, "%d:%d:%d", r, g, b) == 3)
	{
	    if (*r<0 || *r>255 || *g<0 || *g>255 || *b<0 || *b>255) {
		badrule(tty,buf,line);
		continue;
	    }
	    *val = 0;
	    *set = 0;
	    return 1;
	}
#ifdef BACKWARDS_COMPAT
	if (sscanf (buf, "%d %d %d", r, g, b) == 3)
	{
	    if (*r<0 || *r>255 || *g<0 || *g>255 || *b<0 || *b>255) {
		badrule(tty,buf,line);
		continue;
	    }
	    *val = 0;
	    *set = 0;
	    return 1;
	}
#endif
*/
	if (sscanf (buf, "%lf%% %s", &n, color) == 2)
	{
	    if (!lookup_color(tty, color, r,g,b) || n < 0 || n > 100)
	    {
		badrule(tty,buf,line);
		continue;
	    }
	    *val = min + ((double)max-(double)min)*n/100.0;
	    *set = 1;
	    return 1;
	}
	if (sscanf (buf, "%lf %s", val, color) == 2)
	{
	    if (!lookup_color(tty, color, r,g,b))
	    {
		badrule(tty,buf,line);
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
            if (!lookup_color(tty, color, r,g,b))
            {
                badrule(tty,buf,line);
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
	    	fprintf(stderr, _("** rule is not added **\n"));
	    	continue;
	    }
        }

	/* does this actually do anything?? */
/*	if (sscanf (buf, "%s", color) == 1)
	{
	    if (!lookup_color(tty, color, r,g,b))
	    {
		badrule(tty,buf,line);
		continue;
	    }
	    *val = 0;
	    *set = 0;
	    return 1;
	} */

	badrule(tty,buf,line);
    }
    return 0;
}

static int badrule(int tty, char *s, int line)
{
    if (!tty)
	fprintf (stderr, _("%s:line %d:%s\n"), G_program_name(), line,  s);
    fprintf (stderr, _("** bad color specification **\n"));
    fprintf (stderr, _("** rule is not added **\n"));
    if (!tty)
	exit(1);

    return 0;
}

static int lookup_color(int tty, char *color, int *r, int *g, int *b)
{
    float fr, fg, fb;

    if (G_color_values(color, &fr, &fg, &fb) > 0)
    {
	*r = (int)(fr * 255);
	*g = (int)(fg * 255);
	*b = (int)(fb * 255);
	return 1;
    }
    if (tty)
    {
	fprintf (stderr, _("%s - unknown color\n"), color);
	fprintf (stderr, _("Valid colors are:\n"));
	show_colors (stderr);
    }
    return 0;
}

static int show_colors (FILE *fp)
{
    int len;
    int i,n;
    char *color;

    len = 0;
    for (i = 0; color = G_color_name(i); i++)
    {
	n = strlen (color) + 1;
	if (len + n > 78)
	{
	    fprintf (fp, "\n");
	    len = 0;
	}
	fprintf (fp, " %s", color);
	len += n;
    }
    fprintf (fp, "\n");
    
    return 0;
}
