#include "global.h"

static struct sort_stats
{
    CELL *cat;
    STATS *stats;
    int k;
} *sort_stats;

results (fmt, non_zero, cell_counts, planimetric, unit_area)
    char *fmt;
    double unit_area;
{
    CELL *cat;
    int p;
    STATS *stats;
    int i;
    int k;
    int n;
    int zero;
    int cmp();
    int nstats;
    int pass;

/*
 * transfer the tree of stats into a an array for sorting
 * first pass thru tree simply counts the number of entries
 * second pass allocates the array and inserts the entries into the array
 */

    for (pass = 0; pass < 2; pass++)
    {
	if (pass != 0)
	    sort_stats = (struct sort_stats *) G_calloc (nstats, sizeof (struct sort_stats));
	nstats = 0;
	for (p=first_node(&cat, &stats); p > 0; p=next_node(p, &cat, &stats))
	{
	    for (k = 0; k < NCATS; k++)
	    {
		if (stats[k].count == 0) continue;
		if (non_zero)
		{
		    zero = 1;
		    if (cat[0]+k) zero = 0;
		    else
			for (i = 1; zero == 1 && i < nfiles; i++)
			    if (cat[i])
				zero = 0;
		    if (zero) continue;
		}
		if (pass != 0)
		{
		    sort_stats[nstats].cat = cat;
		    sort_stats[nstats].stats = stats;
		    sort_stats[nstats].k = k;
		}
		nstats++;
	    }
	}
	if (!nstats) /* very special case here */
	{
	    for (i = 0; i < nfiles; i++)
		printf ("0:");
	    if (cell_counts == 1)
		printf ("0");
	    else
		printf ("0.0");
	    if (cell_counts == 2)
		printf (":0");
	    printf ("\n");
	    return;
	}
    }

/* sort the array */

    qsort (sort_stats, nstats, sizeof(struct sort_stats), cmp);

/* print the results
 * the primary file may not have been the first on the command line
 * print the results in the order that the files were specified
 */
    for (n = 0; n < nstats; n++)
    {
	cat = sort_stats[n].cat;
	stats = sort_stats[n].stats;
	k = sort_stats[n].k;
	for (i = 1; i <= primary; i++)
	    printf ("%ld:", (long) cat[i]);
	printf ("%ld:",(long)cat[0]+k);
	for (i = primary+1; i < nfiles; i++)
	    printf ("%ld:", (long)cat[i]);

    /* now the counts/areas */
	if (cell_counts == 1)
	    printf ("%ld", (long)stats[k].count);
	else if (planimetric)
	    printf (fmt, (double) stats[k].count*unit_area);
	else
	    printf (fmt, (double) stats[k].area);
	if (cell_counts == 2)
	    printf (":%ld", (long)stats[k].count);
	printf ("\n");
    }
}

static
cmp (a, b)
    struct sort_stats *a, *b;
{
    register int i, diff;
    for (i = 1; i <= primary; i++)
	if (diff = (a->cat[i] - b->cat[i])) return diff;
    if (diff = (a->cat[0]+a->k - b->cat[0]-b->k)) return diff;
    for (;i < nfiles;i++)
	if (diff = (a->cat[i] - b->cat[i])) return diff;
    return 0;
}
