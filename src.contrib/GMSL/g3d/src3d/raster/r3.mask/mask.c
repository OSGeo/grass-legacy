#include "gis.h"

typedef struct _d_interval {
	double low, high;
	int inf;
	struct _d_interval *next;
} d_Interval;

typedef struct _d_mask {
	d_Interval *list;
} d_Mask;

static
init_d_mask_rules (d_mask)
    d_Mask *d_mask;
{
    d_mask->list = NULL;
}

static
add_d_mask_rule (d_mask, a, b, inf)
    d_Mask *d_mask;
    double a, b;
    int inf;
{
    d_Interval *I;

    I = (d_Interval *) G_malloc (sizeof(d_Interval));
    I->low  = a <= b ? a : b ;
    I->high = a >= b ? a : b ;
    I->inf  = inf;
    I->next = d_mask->list;
    d_mask->list = I;
}

mask_d_select (x, mask)
    DCELL *x;
    d_Mask *mask;
{
    d_Interval *I;

    if (mask->list == NULL) return 0;
    for (I = mask->list; I; I = I->next)
    {
	if (mask_match_d_interval (*x, I))
	    return 1;
    }
    return 0;
}

extern
mask_match_d_interval (x, I)
    DCELL x;
    d_Interval *I;
{
    if (I->inf < 0)
	return x <= I->low;

    if (I->inf > 0)
	return x >= I->high;

    return x >= I->low && x <= I->high;
}

static
parse_d_mask_rule (vallist, d_mask, where)
    char *vallist;
    d_Mask *d_mask;
    char *where;
{
    double a,b;
    char junk[128];

/* #-# */
    if (sscanf (vallist,"%lf-%lf",&a,&b) == 2)
    {
	printf("adding rule: %lf - %lf\n", a,b);
	add_d_mask_rule (d_mask, a, b, 0);
    }
/* inf-# */
    else if (sscanf (vallist,"%[^ -\t]-%lf", junk, &a) == 2)
	add_d_mask_rule (d_mask, a, a, -1);

/* #-inf */
    else if (sscanf (vallist,"%lf-%[^ \t]", &a, junk) == 2)
	add_d_mask_rule (d_mask, a, a, 1);

/* # */
    else if (sscanf (vallist,"%lf",&a) == 1)
	add_d_mask_rule (d_mask, a, a, 0);

    else
    {
	if(where)
	    fprintf (stderr, "%s: ", where);
	fprintf (stderr, "%s: illegal value spec\n", vallist);
	G_usage();
	exit(1);
    }
}

parse_vallist (vallist, d_mask)
    char **vallist;
    d_Mask **d_mask;
{
    char buf[1024];
    char x[2];
    FILE *fd;

    *d_mask = (d_Mask *) G_malloc (sizeof (d_Mask));

    init_d_mask_rules (*d_mask);
    if (vallist == NULL) return;

    for ( ; *vallist; vallist++)
    {
	if (*vallist[0] == '/')
	{
	    fd = fopen (*vallist, "r");
	    if (fd == NULL)
	    {
		perror (*vallist);
		G_usage();
		exit(1);
	    }
	    while (fgets (buf, sizeof buf, fd))
	    {
		if (sscanf (buf, "%1s", x) != 1 || *x == '#')
		    continue;
		parse_d_mask_rule (buf, *d_mask, *vallist);
	    }
	    fclose(fd);
	}
	else
	    parse_d_mask_rule (*vallist, *d_mask, (char *)NULL);
    }
}

